# load the data.table package
library(data.table)
# load the lubridate package
library(lubridate)
# load the dplyr package to enable use of the pipe operator
library(dplyr)
# read file with stock data
stockdata<-fread("./data/stock_data.csv")
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
ranking_start<-stockdata$date %m-% months(12) # subtract twelve months
day(ranking_start)<-days_in_month(ranking_start) # set day of the date = last day of the month
stockdata$ranking_start<-ranking_start

# calculate end of the formation period for each date
ranking_end<-stockdata$date %m-% months(1) # subtract one month
day(ranking_end)<-days_in_month(ranking_end) # set day of the date = last day of the month
stockdata$ranking_end<-ranking_end
stockdata

# calculate cumulative log returns for each row
# define function 
csum = function(permno_input, startdate, enddate){ 
    sum((stockdata %>% filter(permno == permno_input & date >= startdate & date <= enddate))$value)
}

csum = Vectorize(csum)

#execute calculation
stockdata %>% mutate(cum_log_return = csum(permno, ranking_start, ranking_end))
stockdata

# calculate number of stock returns available for the past 11 months 
fcount = function(permno_input, startdate, enddate){
    length((stockdata %>% filter(permno == permno_input & date >= startdate & date <= enddate)))
}

fcount = Vectorize(fcount)

#execute counting
stockdata %>% mutate(available_returns = fcount(permno, ranking_start, ranking_end))
stockdata

# TODO: next step would be to group data by date, determine the shares that have available_returns>=8 for each date
# as specified in the paper and then divide these into ten deciles before extracting the winners (top decile) and
# losers (bottom decile)