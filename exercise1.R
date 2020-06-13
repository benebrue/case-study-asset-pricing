# load the data.table package
library(data.table)
# load the lubridate package
library(lubridate)
# load the dplyr package to enable use of the pipe operator
library(dplyr)
# load the rugarch package which will be used for variance prediction
library(rugarch)
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


#create column for log returns
stockdata[,log_return:=log(1+return)]
stockdata

# calculate start of the formation period for each date
ranking_start<-stockdata$date %m-% months(11) # subtract eleven months because the return stored as that of the 
# point in time 11 months ago is the one realized starting 12 months ago
day(ranking_start)<-days_in_month(ranking_start) # set day of the date = last day of the month
stockdata$ranking_start<-ranking_start

# calculate end of the formation period for each date
ranking_end<-stockdata$date %m-% months(1) # subtract one month
day(ranking_end)<-days_in_month(ranking_end) # set day of the date = last day of the month
stockdata$ranking_end<-ranking_end
stockdata

# for testing the start/end dates
# sdate<-stockdata[,.(ranking_start)][11]
# edate<-stockdata[,.(ranking_end)][11]
# stockdata[permno == 10001,] %>% filter(date >= sdate & date <= edate)

# calculate cumulative log returns for each row
# define function
csum = function(permno_input, startdate, enddate){
    sum((stockdata[permno == permno_input,] %>% filter(date >= startdate & date <= enddate))$log_return)
}

csum = Vectorize(csum)

#execute calculation
stockdata$cum_log_return<-mapply(csum, stockdata$permno, stockdata$ranking_start, stockdata$ranking_end)
stockdata

# calculate number of stock returns available for the past 11 months 
fcount = function(permno_input, startdate, enddate){
    nrow((stockdata %>% filter(permno == permno_input & date >= startdate & date <= enddate)))
}

fcount = Vectorize(fcount)

#execute counting
stockdata$available_returns<-mapply(fcount, stockdata$permno, stockdata$ranking_start, stockdata$ranking_end)
stockdata

# remove rows which have available_returns < 8
stockdata<-stockdata[available_returns >= 8,]
stockdata

# cut returns for each date into deciles to determine winner/loser portfolios
stockdata[,bin:=cut(cum_log_return,
                    quantile(cum_log_return,probs=seq(from=0,to=1,by=1/10),na.rm =T),
                    include.lowest=TRUE, 
                    labels=FALSE),
          by=date]
stockdata

# get data of the stocks in the top decile
top<-stockdata[bin == 10]
#calculate sum of the market capitalization for each date
top$tcapsum<-ave(top$tcap, top$date, FUN=sum)
top
# get data of the stocks in the bottom decile
bottom<-stockdata[bin == 1]
#calculate sum of the market capitalization for each date
bottom$tcapsum<-ave(bottom$tcap, bottom$date, FUN=sum)
bottom

# group by date and calculate value-weighted return
top<-top %>% group_by(date) %>%
    summarise(vw_return = sum(return * tcap / tcapsum))
top

# convert back from a tibble to a data.table
setDT(top)
top

bottom<-bottom %>% group_by(date) %>%
    summarise(vw_return = sum(return * tcap / tcapsum))
bottom
# convert back from a tibble to a data.table
setDT(bottom)
bottom

portfolios<-merge(bottom, top, by.x="date", by.y="date")
colnames(portfolios)<-c("date", "return_bottom", "return_top")
portfolios

portfolios[,return_wml:=return_top-return_bottom]
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

# calculate total market capitalization for each date
stockdata2$tcapsum<-ave(stockdata2$tcap, stockdata2$date, FUN=sum)

# group by date and calculate value-weighted return
stockdata2<-stockdata2 %>% group_by(date) %>%
    summarise(vw_return = sum(return * tcap / tcapsum))
stockdata2

# convert back from a tibble to a data.table
setDT(stockdata2)
stockdata2

# calculate the start of a 126-day time period for each date which will be used to estimate the variance (see
# page 230 of the paper)
stockdata2[, calc_start:= date %m-% days(126)]
stockdata2

# calculate the variance for each date using the specified periods of time
var_vec = function(startdate, enddate){
    temp<-(stockdata2 %>% filter(date >= startdate & date <= enddate))$vw_return
    result<-ifelse(length(temp)>0, var(temp), NA) # return var if more than one vw_return exists, else return NA
}

var_vec = Vectorize(var_vec)

# execute variance calculation and remove rows that have no variance (NA)
stockdata2$var<-mapply(var_vec, stockdata2$calc_start, stockdata2$date)
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

garchspec<-ugarchspec(
    mean.model=list(armaOrder=c(0,0), include.mean=TRUE),
    variance.model=list(model="gjrGARCH"),
    distribution.model="sstd"
)
garchfit<-ugarchfit(data=vdata_xts, spec=garchspec)
sigma(garchfit)
