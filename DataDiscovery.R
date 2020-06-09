#install packages
#install.packages("data.table")
#install.packages("lubridate")
#install.packages("dplyr")
install.packages("ggplot2")

#load Packages
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)

#load and formate stock data
stockdata <- fread("./data/stock_data.csv")
stockdata <- stockdata[complete.cases(stockdata), ]
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
#stockdata <- stockdata %>% group_by(permno) %>% filter(n() >= 8)

#no. of shares in stock universe
no_stock <- length(unique(stockdata$permno))
no_stock


#****************************
#Implementation of strategy
#****************************
#Testing

stockdata<-stockdata[1:9000,]


#Checkpoint to save Workspace
save.image(file='./wksp/checkpoint_1.RData')
load('./wksp/checkpoint_1.RData')


#Find monthly momentum portfolio based on paper (momentum crashes)
#select momentum portfolio by:
# (i)   min 8 of 11 past return available
# (ii)  consider  t-12 to t-2 returns to formate portfolio
# (iii) rank stocks and divide in 10 decils (1st - winner, 10th - loser)


#create column for log returns
stockdata[,log_return:=log(1 + return)] #Doesn't work
#stockdata$log_return = log(1+stockdata$return)
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

quantile_breaks <- quantile(stockdata$cum_log_return,probs=seq(from=0,to=1,by=1/10),na.rm =T)
stockdata[,bin:=cut(cum_log_return,
                    quantile_breaks,
                    include.lowest=TRUE, 
                    labels=FALSE),
          by=date]
stockdata

save.image(file='./wksp/checkpoint_2.RData')
load('./wksp/checkpoint_2.RData')

#calculate sum of capitalization for each month and bin 
#to compute weight of shares as value weighted approch is mentioned in the paper
capital_cum <- function(date_input, bin_input){sum((stockdata[date == date_input,]%>%filter(bin==bin_input))$tcap)
}
capital_cum = Vectorize(capital_cum)
stockdata$cum_cap <- mapply(capital_cum,stockdata$date,stockdata$bin)
stockdata
#calculate weight for each line
stockdata$weight <- stockdata$tcap / stockdata$cum_cap
stockdata
#calculate vaule weighted return for each line
stockdata$weighted_return <- stockdata$return * stockdata$weight

#extract returns for each portfolio by date and bin
portfolios <- stockdata %>% 
                select(date,bin,weighted_return) %>%
                group_by(date, bin) %>% 
                summarise_each(funs(sum))



#calculated cumulative return
# portfolios$invest_return <- cumprod(1+portfolios$weighted_return) #doesn't work, first extract pf10



#extract momentum portfolio
portfolio_10 <- filter(portfolios,bin==10)
portfolio_10$invest_return <- cumprod(1+portfolio_10$weighted_return)

ggplot(data=portfolio_10,aes(x=date)) + geom_line(aes(y=invest_return,color="red"))
