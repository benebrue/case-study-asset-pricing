# load the data.table package
library(data.table)
# load the lubridate package
library(lubridate)
# load the dplyr package to enable use of the pipe operator
library(dplyr)
# read file with stock data
stockdata<-fread("./data/stock_data.csv")
#!!!for development purposes only: reduce size of the dataset
stockdata<-stockdata[1:10000,]
stockdata<-stockdata[complete.cases(stockdata), ]
stockdata

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
# and market variance over the preceding six months