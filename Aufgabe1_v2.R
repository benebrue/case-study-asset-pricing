#install packages
install.packages("data.table")
install.packages("lubridate")
install.packages("dplyr")

#load Packages
library(data.table)
library(lubridate)
library(dplyr)

#load and formate stock data
stockdata<-fread("./data/stock_data.csv")
stockdata
lapply(stockdata, class)
#date as date
stockdata[,date:=as.Date(date,format="%Y-%m-%d")]
lapply(stockdata, class)

#no. of shares in stock universe
#no_stock <- length(unique(stockdata$permno))
#no_stock

#Find monthly momentum portfolio based on paper (momentum crashes)
#select momentum portfolio by:
# (i)   min 8 of 11 past return available
# (ii)  consider  t-12 to t-2 returns to formate portfolio
# (iii) rank stocks and divide in 10 decils (1st - winner, 10th - loser)

#use of log returns like in the paper
#create cloumn for log returns
stockdata$log_return = log(1+stockdata$return)


#find formation period
#start t-12 months
formation_start<-stockdata$date %m-% months(12)
stockdata$formation_start<-formation_start
#end t-2 months
formation_end<-stockdata$date %m-% months(1)
stockdata$formation_end<-formation_end

#cummulate returns for formation period

#find number of returns in formation period 
count_returns <- function(permno_input, start_input, end_input){
  length((stockdata %>% filter(permno==permno_input & date>=start_input & date<=end_input)))
}
count_returns <- Vectorize(count_retruns)
stockdata <- stockdata %>% mutate(no_retruns = count_returns(permno, formation_start, formation_end))

#drop rows with not enough available returns to reduce data
stockdata <- filter(stockdata, no_returns >= 8)

#function to sum up filtered by permno, and formation period
sum_return <- function(permno_input, start_input, end_input){
  sum((stockdata %>% filter(permno==permno_input & date>=start_input & date<=end_input))$value)
}
sum_return <- Vectorize(sum_return)
stockdata_1 <-stockdata %>% mutate(sum_log_return = sum_return(permno, formation_start, formation_end))


#stockdata %>% separate(permno, into, sep = " ", remove = TRUE, convert = TRUE)

melt(stockdata,"date")











