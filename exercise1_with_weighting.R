# load the data.table package
library(data.table)
# load the lubridate package
library(lubridate)
# load the dplyr package to enable use of the pipe operator
library(dplyr)
# load the rugarch package which will be used for variance prediction
library(rugarch)
# load the matrixStats library to enable use of the rowProds operation
library(matrixStats)
# load the ggplot library to enable plotting
library(ggplot2)
# read file with stock data
stockdata<-fread("./data/stock_data.csv")
#!!!for development purposes only: reduce size of the dataset
stockdata<-stockdata[1:10000,]
stockdata<-stockdata[complete.cases(stockdata), ]
stockdata
# TODO: implement filtering of the data as in DataDiscovery

# implement a monthly momentum portfolio (daily version described in the paper would require information on daily 
# stock returns which we don't have)
# there must be at least 8 returns over the past 11 months for a stock to be considered
# use the t-12 to t-2 month returns (one-month gap between end of ranking period and start of holding period)
# divide shares into ten deciles, use top decile (winners) and bottom decile (losers)
# compute the value-weighted holding period returns of the decile portfolios
# market return = value-weighted index of all listed firms
# risk-free rate = one-month Treasury bill rate
# use cumulative log returns for the ranking (see http://kentdaniel.net/data/momentum/mom_data.pdf)

# check classes of columns
lapply(stockdata, class)
# date is currently a char value --> convert date to an actual date
stockdata[,date:=as.Date(date,format="%Y-%m-%d")]
lapply(stockdata, class)


# calculate start of the formation period for each date
stockdata$ranking_start<-stockdata$date %m-% months(12) # subtract 12 months
day(stockdata$ranking_start)<-days_in_month(stockdata$ranking_start) # set day of the date = last day of the month

# calculate end of the formation period for each date
stockdata$ranking_end<-stockdata$date %m-% months(2) # subtract two months
day(stockdata$ranking_end)<-days_in_month(stockdata$ranking_end) # set day of the date = last day of the month

# save market capitalization to use for weighting as tcap_shifted
stockdata2<-stockdata # copy stockdata
stockdata2$shifted_date<-stockdata2$date %m+% months(1) # shift by one month
day(stockdata2$shifted_date)<-days_in_month(stockdata2$shifted_date)
stockdata<-merge(stockdata, # merge tables using the shifted date
                 stockdata2[,.(permno, shifted_date, tcap)],
                 by.x=c("permno", "date"), 
                 by.y=c("permno", "shifted_date"),
                 all.x=TRUE)
names(stockdata)[names(stockdata) == "tcap.x"] <- "tcap"
names(stockdata)[names(stockdata) == "tcap.y"] <- "tcap_shifted"
stockdata[is.na(stockdata)]<-0 # set NA values to zero (no tcap available --> not considered in the portfolio)
stockdata

# add the returns which the ranking will be based on as separate columns
return_cols<-c()
for(i in c(2:12)){
    stockdata2$shifted_date<-stockdata2$date %m+% months(i) # shift by i months
    day(stockdata2$shifted_date)<-days_in_month(stockdata2$shifted_date)
    stockdata<-merge(stockdata, # merge return using the shifted date
                     stockdata2[,.(permno, shifted_date, return)],
                     by.x=c("permno", "date"),
                     by.y=c("permno", "shifted_date"),
                     all.x=TRUE
    )
    names(stockdata)[names(stockdata) == "return.x"] <- "return" # correct the column names
    names(stockdata)[names(stockdata) == "return.y"] <-paste0("return_m", i)
    return_cols<-c(return_cols, paste0("return_m", i))
}
stockdata
return_cols
stockdata$available_returns<-rowSums(!is.na(stockdata[,return_cols, with=FALSE])) # compute number of available returns
stockdata

stockdata2<-NULL

stockdata$cum_return<-rowProds(1+as.matrix(stockdata[,return_cols, with=FALSE]), na.rm=T) # compute cumulative product
stockdata
stockdata<-stockdata[, !return_cols, with=FALSE] # drop return_cols (not needed anymore)
return_cols<-NULL
stockdata

# remove rows which have available_returns < 8
stockdata<-stockdata[available_returns >= 8,]
stockdata

# normalize using the number of available returns
stockdata$cum_return<-stockdata$cum_return / stockdata$available_returns
# cut returns for each date into deciles to determine winner/loser portfolios
stockdata[,bin:=cut(cum_return,
                    quantile(cum_return,probs=seq(from=0,to=1,by=1/10),na.rm =T),
                    include.lowest=TRUE, 
                    labels=FALSE),
          by=date]


# get data of the stocks in the top decile
top<-stockdata[bin == 10]
#calculate sum of the market capitalization for each date
top$tcapsum<-ave(top$tcap_shifted, top$date, FUN=sum)
top
# get data of the stocks in the bottom decile
bottom<-stockdata[bin == 1]
#calculate sum of the market capitalization for each date
bottom$tcapsum<-ave(bottom$tcap_shifted, bottom$date, FUN=sum)
bottom


# group by date and calculate value-weighted return
top<-top %>% group_by(date) %>%
    summarise(vw_return = sum(return * tcap_shifted / tcapsum))
top

# convert back from a tibble to a data.table
setDT(top)
top

bottom<-bottom %>% group_by(date) %>%
    summarise(vw_return = sum(return * tcap_shifted / tcapsum))
bottom
# convert back from a tibble to a data.table
setDT(bottom)
bottom

portfolios<-merge(bottom, top, by.x="date", by.y="date")
colnames(portfolios)<-c("date", "return_bottom", "return_top")
portfolios


#plotting
portfolios$cum_bottom<-cumprod(1+portfolios$return_bottom)
portfolios$cum_top<-cumprod(1+portfolios$return_top)

ggplot(data=portfolios,aes(x=date)) + geom_line(aes(y=cum_bottom,color="bottom")) + geom_line(aes(y=cum_top,color="top"))

# if we want to calculate the monthly returns of the WML portfolio, we also need to know the risk-free rate since the
# margin that we post as we implement our strategy earns interest at this rate
ffdata<-fread("./data/F-F_Research_Data_Factors.csv")
ffdata$RF<-ffdata$RF/100 # convert from percentage values to actual values
ffdata[,date:=as.Date(paste(year, month, "01", sep="-"))]
day(ffdata$date)<-days_in_month(ffdata$date)
ffdata
ffdata<-ffdata[,.(date, RF)]
ffdata


# merge table containing the risk-free returns with the portfolios table
portfolios<-merge(portfolios, ffdata, by.x="date", by.y="date")
portfolios

# calculate return of the WML portfolio for each month (see Appendix A of the paper)
portfolios[,return_wml:=return_top-return_bottom+RF]
portfolios

# at this the step "portfolios" contains the value-weighted returns of the top 10% portfolio, the value-weighted
# returns of the bottom 10% portfolio and the returns of the WML strategy (long top, short bottom)


# next step is to calculate the weights necessary for balancing WML portfolio and risk-free asset
# to calculate the weights, we need conditional expected return and conditional variance for the coming month

# proxy for conditional expected return: fitted regression of WML returns on interaction between bear market indicator
# and market variance over the preceding six months --> for this regression, we need the variance and the bear
# market indicator

# estimate the variance of the market --> to do this, load the stockdata again and build the (value-weighted) market
# portfolio

# read file with stock data
stockdata2<-fread("./data/stock_data.csv")
#!!!for development purposes only: reduce size of the dataset
stockdata2<-stockdata2[1:10000,]
stockdata2<-stockdata2[complete.cases(stockdata2), ]
stockdata2

# check classes of columns
lapply(stockdata2, class)
# date is currently a char value --> convert date to an actual date
stockdata2[,date:=as.Date(date,format="%Y-%m-%d")]
lapply(stockdata2, class)

stockdata3<-stockdata2 # copy stockdata
stockdata3$shifted_date<-stockdata3$date %m+% months(1) # shift by one month
day(stockdata3$shifted_date)<-days_in_month(stockdata3$shifted_date)
stockdata2<-merge(stockdata2, # merge tables using the shifted date
                 stockdata3[,.(permno, shifted_date, tcap)],
                 by.x=c("permno", "date"), 
                 by.y=c("permno", "shifted_date"),
                 all.x=TRUE)
names(stockdata2)[names(stockdata2) == "tcap.x"] <- "tcap"
names(stockdata2)[names(stockdata2) == "tcap.y"] <- "tcap_shifted"
stockdata2[is.na(stockdata2)]<-0 # set NA values to zero (no tcap available --> not considered in the portfolio)
stockdata2

# calculate total market capitalization for each date
stockdata2$tcapsum<-ave(stockdata2$tcap_shifted, stockdata2$date, FUN=sum)

# group by date and calculate value-weighted return
stockdata2<-stockdata2 %>% group_by(date) %>%
    summarise(vw_return = sum(return * tcap_shifted / tcapsum))
stockdata2

# convert back from a tibble to a data.table
setDT(stockdata2)
stockdata2

# calculate the start of a 126-day time period for each date which will be used to estimate the variance (see
# page 230 of the paper)
stockdata2$calc_start<-stockdata2$date %m-% months(1) # shift one month back to account for time at which investment
# decision is made
day(stockdata2$calc_start)<-days_in_month(stockdata2$date)
stockdata2[, calc_end:= calc_start %m-% days(126)]
stockdata2

# calculate the variance for each date using the specified periods of time
var_vec = function(startdate, enddate){
    temp<-(stockdata2 %>% filter(date >= startdate & date <= enddate))$vw_return
    result<-ifelse(length(temp)>0, var(temp), NA) # return var if more than one vw_return exists, else return NA
}

var_vec = Vectorize(var_vec)

# execute variance calculation and remove rows that have no variance (NA)
stockdata2$var<-mapply(var_vec, stockdata2$calc_start, stockdata2$calc_end)
stockdata2
stockdata2<-stockdata2[complete.cases(stockdata2), ]
stockdata2

# load recession indicator data
recessiondata<-fread("./data/nber_recessions.csv")
recessiondata
# check classes of columns
lapply(recessiondata, class)
# date is currently a char value --> convert date to an actual date
recessiondata[,date:=as.Date(date,format="%Y-%m-%d")]
lapply(recessiondata, class)

# opposed to the data description, the first day instead of the last day is specified for each date --> change this
day(recessiondata$date)<-days_in_month(recessiondata$date) # set day of the date = last day of the month

# merge recessiondata with stockdata
stockdata2<-merge(stockdata2, recessiondata, by.x="date", by.y="date", all.x=TRUE)
stockdata2

# remove NA rows (interim solution, can be removed once filtering of the data as in DataDiscovery has been added
stockdata2<-stockdata2[complete.cases(stockdata2), ]
stockdata2
portfolios

# create data table for the expected return and variance regression (which will be used for weighting)
# we need to shift the date for the stockdata2 one month forward before merging it with portfolios so that the 
# variance and bear market indicator of the current month is used to predict the return of the WML
erdata<-stockdata2[,shifted_date:=date %m+% months(1)]
day(erdata$shifted_date)<-days_in_month(erdata$shifted_date)
erdata
erdata<-merge(erdata, portfolios[,.(date, return_wml)], by.x="shifted_date", by.y="date") # inner join
erdata$calc_start<-NULL
erdata$shifted_date<-NULL
erdata$vw_return<-NULL
erdata

# now calculate the date 6 months ago for each row so we know which date range the regression must be executed for
erdata[,reg_start:=date %m-% months(6)]
day(erdata$reg_start)<-days_in_month(erdata$reg_start)
erdata

# function for expected return regression
erreg = function(startdate, enddate){
    temp<-(erdata %>% filter(date >= startdate & date < enddate)) # <enddate so current month (for which return
    # will be predicted) isn't already used for the estimation of the regression parameters
    result<-ifelse(length(temp)==6, predict(lm(return_wml ~ recession + var + recession*var, data=temp),
                                            newdata=erdata[date==enddate,]), NA) 
    # return predicted return for the next month if there are 6 preceding months that the regression can be executed
    # on, else return NA
}

erreg = Vectorize(var_vec)

# apply the linear regression for each 6-month time window and predict next month's return
erdata$return_wml_pred<-mapply(erreg, erdata$reg_start, erdata$date)
erdata

# next step is fitting a GJR-GARCH model on the data to estimate the variance in an upcoming period of time
# to do so, we will use the rugarch package (https://cran.r-project.org/web/packages/rugarch/index.html)
# library(PerformanceAnalytics)
# chart.Histogram(erdata$return_wml, methods = c("add.normal", "add.density"), colorset=c("gray","red","blue"))
# from the chart we can see that the distribution of our returns has a high peak around 0, fat tails and is skewed
# --> use skewed student t distribution instead of normal distribution  ("sstd)

# create a table containing the data for fitting the GJR-GARCH model
vdata<-portfolios
vdata$return_bottom<-NULL
vdata$return_top<-NULL
vdata

# convert vdata to a timeseries object which the rugarch package is optimized for
vdata_xts<-as.xts.data.table(vdata)
vdata_xts

garchspec<-ugarchspec(
    mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
    variance.model=list(model="gjrGARCH"),
    distribution.model="sstd"
)
garchfit<-ugarchfit(data=vdata_xts, spec=garchspec)
coef(garchfit)
v_pred_xts<-sigma(garchfit) # get standard deviations predicted by the GJR-GARCH model
v_pred<-as.data.table(v_pred_xts) #convert back to data table
colnames(v_pred)<-c("date", "v_pred")
v_pred
# convert standard deviation to variance
v_pred$v_pred<-v_pred$v_pred^2
v_pred

# compute one-month lag for the variance (since for the weighting we want to use next month's predicted variance for
# today's weight)
v_pred<-v_pred[,shifted_date:=date %m-% months(1)]
day(v_pred$shifted_date)<-days_in_month(v_pred$shifted_date)
v_pred
weightdata<-merge(erdata, v_pred, by.x="date", by.y="shifted_date") # inner join
weightdata
weightdata$date.y<-NULL
weightdata$reg_start<-NULL
weightdata$var<-NULL
weightdata$recession<-NULL
weightdata

# in the paper, Daniel and Moskowitz use another regression (variance of future 22-day returns on the GJR-GARCH 
# estimate of the variance and the variance within the past 126 days) to predict the future variance
# as we don't have information about daily returns, we can't compute the future 22-day variance and therefore cannot
# compute this regression --> we will simply use the GJR-GARCH variance estimates for weighting

# now we need to determine lambda in such a way that the variance of our momentum portfolio is equal to the variance
# of the value-weighted return of the market

# calculate the variance of the market
v_market<-var(filter(stockdata2, date<=max(v_pred$date) & date>=min(v_pred$date))$vw_return)
v_market

# calculate returns of the weighted momentum strategy assuming lambda is known
# in weightdata, return_wml, return_wml_pred and v_pred are the realized return, predicted return and predicted
# variance in the month after the date specified in column "date"
weightdata
l<-1

# note that the paper assumes (see Appendix A) that shortselling stocks with a value of x only requires an amount of
# x to be deposited as margin (unlike actual regulations in the US that nowadays require 150% of x as margin)
# therefore if one has 1$, use the 1$ as margin for shorting 1$ of the losers (which yields 1$) and use that money
# to go long on 1$ of the winners
# to calculate the variance we also need to know the risk-free rate of the upcoming month
ffdata[,shifted_date:=date %m-% months(1)]
day(ffdata$shifted_date)<-days_in_month(ffdata$shifted_date)
ffdata
weightdata<-merge(weightdata, ffdata, by.x="date", by.y="shifted_date")
weightdata
weightdata$date.y<-NULL
weightdata
weightdata$weight<-1/(2*l)*weightdata$return_wml_pred/weightdata$v_pred
weightdata

# calculate the return of the dynamic portfolio
weightdata[,return_dyn_pf:=weight*return_wml+(1-weight)*RF]
weightdata
var(weightdata$return_dyn_pf)

# we actually need to set lambda so that the variance of our portfolio is equal to v_market (variance of the market)
# define a function that returns (var_market - var_dynamic_portfolio) and find the roots of this equation
var_function = function(lambda, weights=weightdata){
    weights$weight<-1/(2*lambda)*weights$return_wml_pred/weights$v_pred
    weights[,return_dyn_pf:=weight*return_wml+(1-weight)*RF]
    print(weights)
    var(weights$return_dyn_pf) - v_market
}

#--------------necessary for plotting the function-------------
# var_function_vec<-Vectorize(var_function)
# curve(var_function_vec, from = -3, to = 3); abline(h = 0, lty = 3)
# the plot shows roots around zero with the function value being positive around zero and negative for -infinity and
# infinity --> we decide to prefer a positive lambda value and therefore choose the interval (0.01, 100) for uniroot
# (function is not defined at lambda=0)
#--------------------------------------------------------------

ursolution<-uniroot(var_function, interval=c(0.01, 100), tol=1e-10, maxiter=1000, trace=1)
lambda<-ursolution$root
lambda
ursolution$f.root # function value at the root found --> should be zero or close to zero
ursolution$iter # number of iterations
ursolution$estim.prec # estimated precision

# we have thus found the lambda that we will use for our portfolio
weightdata$weight<-1/(2*lambda)*weightdata$return_wml_pred/weightdata$v_pred
weightdata[,return_dyn_pf:=weight*return_wml+(1-weight)*RF]
weightdata
var(weightdata$return_dyn_pf)
v_market
var_function(lambda)

weightdata$cum_returns_dyn<-cumprod(1+weightdata$return_dyn_pf)
weightdata$cum_returns_nondyn<-cumprod(1+weightdata$return_wml)
weightdata

ggplot(data=weightdata,aes(x=date)) + geom_line(aes(y=cum_returns_dyn,color="dynamic_portfolio")) + geom_line(
    aes(y=cum_returns_nondyn,color="nondynamic_portfolio"))
