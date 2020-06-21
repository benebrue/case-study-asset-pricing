#install packages
#install.packages("data.table")
#install.packages("lubridate")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages('pbapply')

#load Packages
library(data.table)
library(lubridate)
library(dplyr)
library(ggplot2)
library(pbapply)

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
#Transform data to months day to months end
dataRecession[,date:=as.Date(date,format="%Y-%m-%d")]
day(dataRecession$date)<-days_in_month(dataRecession$date)

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
setDT(stockdata)

#no. of shares in stock universe
no_stock <- length(unique(stockdata$permno))
no_stock

#Checkpoint to save Workspace
#save.image(file='./wksp/checkpoint_1.RData')
load('./wksp/checkpoint_1.RData')


#****************************
#Implementation of strategy
#****************************


#Testing
#stockdata<-stockdata[1:100000,]


#Find monthly momentum portfolio based on paper (momentum crashes)
#select momentum portfolio by:
# (i)   min 8 of 11 past return available
# (ii)  consider  t-12 to t-2 returns to formate portfolio
# (iii) rank stocks and divide in 10 decils (1st - winner, 10th - loser)


#create column for log returns
stockdata[,log_return:=log(1 + return)] #Doesn't work
stockdata

# calculate start of the formation period for each date
ranking_start<-stockdata$date %m-% months(12) # subtract eleven months because the return stored as that of the 
# point in time 11 months ago is the one realized starting 12 months ago
day(ranking_start)<-days_in_month(ranking_start) # set day of the date = last day of the month
stockdata$ranking_start<-ranking_start

# calculate end of the formation period for each date
ranking_end<-stockdata$date %m-% months(2) # subtract one month
day(ranking_end)<-days_in_month(ranking_end) # set day of the date = last day of the month
stockdata$ranking_end<-ranking_end
stockdata

# for testing the start/end dates
# sdate<-stockdata[,.(ranking_start)][11]
# edate<-stockdata[,.(ranking_end)][11]
# stockdata[permno == 10001,] %>% filter(date >= sdate & date <= edate)


# calculate number of stock returns available for the past 11 months 
# fcount = function(permno_input, startdate, enddate){
#   nrow((stockdata %>% filter(permno==permno_input & date >= startdate & date <= enddate)))
# }
fcount = function(permno_input, startdate, enddate){
  nrow((stockdata[permno ==permno_input] %>% filter(date >= startdate & date <= enddate)))
}



fcount = Vectorize(fcount)

#execute counting
stockdata$available_returns<-pbmapply(fcount, stockdata$permno, stockdata$ranking_start, stockdata$ranking_end)
stockdata

#save.image(file='./wksp/checkpoint_1_full.RData')
load('./wksp/checkpoint_1_full.RData')

# calculate cumulative log returns for each row
# define function
csum = function(permno_input, startdate, enddate){
  sum((stockdata[permno == permno_input,] %>% filter(date >= startdate & date <= enddate))$log_return)
}

csum = Vectorize(csum)

#execute calculation
stockdata$cum_log_return<-pbmapply(csum, stockdata$permno, stockdata$ranking_start, stockdata$ranking_end)
stockdata


#as we need tcap of previous month for value-weight

tcap_prev = function(permno_input, input_date){
  sum((stockdata[permno == permno_input,] %>% filter(date==input_date))$tcap)
}

tcap_prev = Vectorize(tcap_prev)
stockdata$prev_month <- stockdata$date %m-% months(1)
day(stockdata$prev_month) <- days_in_month(stockdata$prev_month)
stockdata$tcap_prev<-pbmapply(tcap_prev, stockdata$permno, stockdata$prev_month)
stockdata
stockdata[is.na(stockdata)]<-0
stockdata$tcap <- stockdata$tcap_prev



# remove rows which have available_returns < 8
stockdata<-stockdata[available_returns >= 8,]

# cut returns for each date into deciles to determine winner/loser portfolios

# quantile_breaks <- quantile(stockdata$cum_log_return,probs=seq(from=0,to=1,by=1/10),na.rm =T)
# stockdata[,bin:=cut(cum_log_return,
#                     quantile_breaks,
#                     include.lowest=TRUE, 
#                     labels=FALSE),
#           by=date]
stockdata$bin <-NULL
stockdata = stockdata %>% group_by(date) %>% 
  mutate(bin = ntile(cum_log_return, 10))



stockdata



#check deciles for random date and bin1 and bin10
mean(filter(stockdata, bin==1&date=='2012-11-30')$cum_log_return)
mean(filter(stockdata, bin==10&date=='2012-11-30')$cum_log_return)


save.image(file='./wksp/checkpoint_2_full.RData')
load('./wksp/checkpoint_2_full.RData')

#calculate sum of capitalization for each month and bin 
#to compute weight of shares as value weighted approch is mentioned in the paper
#capital_cum <- function(date_input, bin_input){sum((stockdata[date == date_input,]%>%filter(bin==bin_input))$tcap)}
#capital_cum = Vectorize(capital_cum)
#stockdata$cum_cap <- pbmapply(capital_cum,stockdata$date,stockdata$bin)
#stockdata

#calculate weight for each line
#stockdata$weight <- stockdata$tcap / stockdata$cum_cap
#stockdata
#calculate vaule weighted return for each line
#stockdata$weighted_return <- stockdata$return * stockdata$weight

#extract returns for each portfolio by date and bin
#portfolios <- stockdata %>% 
#                select(date,bin,return,tcap) %>%
#                group_by(date, bin) %>% 
#                summarise_each(funs(sum))

#extract top portfolio and calculate tcap sum for each period
portfolio_10 <- filter(stockdata,bin==10)
portfolio_10$tcapsum<-ave(portfolio_10$tcap, portfolio_10$date, FUN=sum)

#calculate value weighted return for each period
portfolio_10<-portfolio_10 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap / tcapsum))

portfolio_10
setDT(portfolio_10)

portfolio_10$invest_return <- cumprod(1+portfolio_10$vw_return)
setDT(portfolio_10)


#extract bottom portfolio and calculate tcap sum for each period
portfolio_1 <- filter(stockdata,bin==1)
portfolio_1$tcapsum<-ave(portfolio_1$tcap, portfolio_1$date, FUN=sum)

#calculate value weighted return for each period
portfolio_1<-portfolio_1 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap / tcapsum))

portfolio_1
setDT(portfolio_1)

portfolio_1$invest_return <- cumprod(1+portfolio_1$vw_return)
setDT(portfolio_1)

#merge tables
portfolios<-merge(portfolio_10, portfolio_1, by.x='date', by.y='date')
portfolios
colnames(portfolios) <- c('date','vw_return_top','invest_return_top','vw_return_bottom','invest_return_bottom')

#merge RF into table
riskfree <- dataFamaFrench
riskfree$mktrf <- NULL
riskfree$smb <- NULL
riskfree$hml <- NULL
riskfree$rf <- riskfree$rf/100
portfolios <- merge(portfolios, riskfree,by.x='date', by.y='date')
portfolios[,invest_rf:=cumprod(1+rf)]


#long winner decile, short loser decil
portfolios$wml_return <- portfolios$vw_return_top-portfolios$vw_return_bottom+portfolios$rf
portfolios$invest_return_wml <- cumprod(1+portfolios$wml_return)


#save.image(file='./wksp/checkpoint_3_full.RData')
load('./wksp/checkpoint_3_full.RData')

portfolios
ggplot(data=portfolios,aes(x=date)) + geom_line(aes(y=invest_return_top,color="Top"))+
                                      geom_line(aes(y=invest_return_bottom,color="Bottom"))+
                                      #geom_line(aes(y=invest_return_wml,color="WML"))+
                                      geom_line(aes(y=invest_rf,color="RF"))

