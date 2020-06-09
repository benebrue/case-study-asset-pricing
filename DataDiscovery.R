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
