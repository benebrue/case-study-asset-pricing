#install packages
#install.packages("data.table")
#install.packages("lubridate")
#install.packages("dplyr")

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

#load fama-french
dataFamaFrench<-fread("./data/F-F_Research_Data_Factors.CSV")
setnames(dataFamaFrench, c("date","year","month","mktrf","smb","hml","rf"))
dataFamaFrench
lapply(dataFamaFrench, class)
dataFamaFrench[,dateHelp:=as.Date(paste0(substr(date,0,4),"-",substr(date,5,6),"-01",format="%Y-%m-%d"))]
dataFamaFrench[,date:=as.Date(paste0(year(dateHelp),"-",month(dateHelp),"-",days_in_month(dateHelp)))]
lapply(dataFamaFrench, class)

#drop columns
dataFamaFrench$year <- NULL
dataFamaFrench$month <- NULL
dataFamaFrench$dateHelp <- NULL

#load nber-recession data
dataRecession<-fread("./data/nber_recessions.csv")
dataRecession
lapply(dataRecession, class)
#TODO: maybe transform data to months day to months end
dataRecession[,date:=as.Date(date,format="%Y-%m-%d")]

#following lines are just for performance, reduce the used data in later steps
#get highest intersection in date
min_date <- max(min(dataRecession$date),min(dataFamaFrench$date),min(stockdata$date))
max_date <- min(max(dataRecession$date),max(dataFamaFrench$date),max(stockdata$date))

#drop rows outside of observed timeframe
stockdata <- filter(stockdata, date<=max_date&date>=min_date)
dataFamaFrench <- filter(dataFamaFrench, date<=max_date&date>=min_date)
dataRecession <- filter(dataRecession, date<=max_date&date>=min_date)

#as we need at least 8 retruns for one id drop all pernom with lower frequency for performance reasons 
stockdata <- stockdata %>% group_by(permno) %>% filter(n() >= 8)

#no. of shares in stock universe
no_stock <- length(unique(stockdata$permno))
no_stock


#****************************
#Implementation of strategy
#****************************
#Testing
stockdata<-stockdata[1:10000,]


#Checkpoint to save Workspace
save.image(file='./wksp/checkpoint_1.RData')
load('./wksp/checkpoint_1.RData')


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
#formation_start<-stockdata$date %m-% months(12)
stockdata$ranking_start<-stockdata$date %m-% months(12)
#end t-2 months
#formation_end<-stockdata$date %m-% months(1)
stockdata$ranking_end<-stockdata$date %m-% months(1)


# calculate number of stock returns available for the past 11 months 
fcount = function(permno_input, startdate, enddate){
  nrow((stockdata %>% filter(permno == permno_input & date >= startdate & date <= enddate)))
}

fcount = Vectorize(fcount)

#execute counting
stockdata$available_returns<-mapply(fcount, stockdata$permno, stockdata$ranking_start, stockdata$ranking_end)
stockdata

# remove rows which have available_returns < 8
stockdata<-filter(stockdata, available_returns >= 8)
stockdata

#Checkpoint to save workspace
save.image(file='./wksp/checkpoint_2.RData')
load('./wksp/checkpoint_2.RData')


# calculate cumulative log returns for each row
# define function
csum = function(permno_input, startdate, enddate){
  sum((stockdata[permno == permno_input,] %>% filter(date >= startdate & date <= enddate))$log_return)
}

csum = Vectorize(csum)

#execute calculation
stockdata$cum_log_return<-mapply(csum, stockdata$permno, stockdata$ranking_start, stockdata$ranking_end)
stockdata


# cut returns for each date into deciles to determine winner/loser portfolios
stockdata[,bin:=cut(cum_log_return,
                    quantile(cum_log_return,probs=seq(from=0,to=1,by=1/10),na.rm =T),
                    include.lowest=TRUE, 
                    labels=FALSE),
          by=date]
stockdata



