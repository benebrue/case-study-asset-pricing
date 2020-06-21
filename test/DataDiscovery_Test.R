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
stockdata_na <- stockdata[!complete.cases(stockdata), ]
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


#****************************
#Implementation of strategy
#****************************
#Testing just take a few stocks
stockdata<- stockdata[1:10000]
setDT(stockdata)
no_stock <- length(unique(stockdata$permno))
no_stock_na <- length(unique(stockdata_na$permno))
no_stock



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
ranking_start<-stockdata$date %m-% months(12) # subtract eleven months because the return stored as that of the 
# point in time 11 months ago is the one realized starting 12 months ago
day(ranking_start)<-days_in_month(ranking_start) # set day of the date = last day of the month
stockdata$ranking_start<-ranking_start

# calculate end of the formation period for each date
ranking_end<-stockdata$date %m-% months(2) # subtract one month
day(ranking_end)<-days_in_month(ranking_end) # set day of the date = last day of the month
stockdata$ranking_end<-ranking_end
stockdata


# calculate number of stock returns available for the past 11 months 
fcount = function(permno_input, startdate, enddate){
  nrow((stockdata %>% filter(permno == permno_input & date >= startdate & date <= enddate)))
}

fcount = Vectorize(fcount)

#execute counting
stockdata$available_returns<-pbmapply(fcount, stockdata$permno, stockdata$ranking_start, stockdata$ranking_end)
stockdata


# calculate cumulative log returns for each row
# define function
# csum = function(permno_input, startdate, enddate){
#   prod(1+(stockdata[permno == permno_input,] %>% filter(date >= startdate & date <= enddate))$return)
# }

csum = function(permno_input, startdate, enddate){
  sum((stockdata[permno == permno_input,] %>% filter(date >= startdate & date <= enddate))$log_return)
}
# csum = function(permno_input, startdate, enddate){
#   sum((stockdata[permno == permno_input,] %>% filter(date >= startdate & date <= enddate))$return)
# }



csum = Vectorize(csum)



#execute calculation
stockdata$cum_log_return<-pbmapply(csum, stockdata$permno, stockdata$ranking_start, stockdata$ranking_end)
stockdata
setDT(stockdata)
#standarize cum_log
#stockdata$cum_log_return <- stockdata$cum_log_return/stockdata$available_returns


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


# remove rows which have available_returns < 8
stockdata<-stockdata[available_returns >=8,]

# cut returns for each date into deciles to determine winner/loser portfolios

stockdata = stockdata %>% group_by(date) %>% 
  mutate(bin = ntile(cum_log_return, 10))
setDT(stockdata)

#check deciles for random date and bin1 and bin10
mean(filter(stockdata, bin==1&date=='2012-11-30')$cum_log_return)
mean(filter(stockdata, bin==10&date=='2012-11-30')$cum_log_return)
nrow(filter(stockdata, bin==9&date=='2012-11-30'))
nrow(filter(stockdata, bin==10&date=='2012-11-30'))
table(t(filter(stockdata, date=='2002-11-30')$bin))

filter(stockdata, date=='2002-11-30')
quantile(filter(stockdata, date=='2002-11-30')$cum_log_return,probs=seq(from=0,to=1,by=1/10),na.rm =T)

##Testing returns of bins
mean_return <- stockdata %>% select(bin,return,cum_log_return) %>% aggregate(list(testdata$bin), mean)
mean_return$Group.1 <- NULL
mx <- t(as.matrix(mean_return[2:3]))
barplot(mx, main="Mean return per bin",xlab="Bins",beside = TRUE,legend=colnames(mean_return[2:3]))


#calculate sum of capitalization for each month and bin 
#to compute weight of shares as value weighted approch is mentioned in the paper
#capital_cum <- function(date_input, bin_input){sum((stockdata[date == date_input,]%>%filter(bin==bin_input))$tcap_prev)}
#capital_cum = Vectorize(capital_cum)
#stockdata$cum_cap <- pbmapply(capital_cum,stockdata$date,stockdata$bin)
#stockdata

#calculate weight for each line
#stockdata$weight <- stockdata$tcap_prev / stockdata$cum_cap
#stockdata
#calculate vaule weighted return for each line
#stockdata$weighted_return <- stockdata$return * stockdata$weight

#extract returns for each portfolio by date and bin
#portfolios <- stockdata %>% 
#                select(date,bin,return,tcap_prev) %>%
#                group_by(date, bin) %>% 
#                summarise_each(funs(sum))

#extract top portfolio and calculate tcap_prev sum for each period
portfolio_10 <- filter(stockdata,bin==10)
portfolio_10$tcap_sum<-ave(portfolio_10$tcap_prev, portfolio_10$date, FUN=sum)

#calculate value weighted return for each period
portfolio_10<-portfolio_10 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcap_sum))

portfolio_10
setDT(portfolio_10)

portfolio_10$invest_return <- cumprod(1+portfolio_10$vw_return)
setDT(portfolio_10)


#extract bottom portfolio and calculate tcap sum for each period
portfolio_1 <- filter(stockdata,bin==1)
portfolio_1$tcapsum<-ave(portfolio_1$tcap_prev, portfolio_1$date, FUN=sum)

#calculate value weighted return for each period
portfolio_1<-portfolio_1 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

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


portfolio_9 <- filter(stockdata,bin==9)
portfolio_9$tcapsum<-ave(portfolio_9$tcap_prev, portfolio_9$date, FUN=sum)

#calculate value weighted return for each period
portfolio_9<-portfolio_9 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

portfolio_9
setDT(portfolio_9)

portfolio_9$invest_return_9 <- cumprod(1+portfolio_9$vw_return)
setDT(portfolio_9)


portfolio_8 <- filter(stockdata,bin==8)
portfolio_8$tcapsum<-ave(portfolio_8$tcap_prev, portfolio_8$date, FUN=sum)

#calculate value weighted return for each period
portfolio_8<-portfolio_8 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

portfolio_8
setDT(portfolio_8)

portfolio_8$invest_return_8 <- cumprod(1+portfolio_8$vw_return)
setDT(portfolio_8)

portfolio_7 <- filter(stockdata,bin==7)
portfolio_7$tcapsum<-ave(portfolio_7$tcap_prev, portfolio_7$date, FUN=sum)

#calculate value weighted return for each period
portfolio_7<-portfolio_7 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

portfolio_7
setDT(portfolio_7)

portfolio_7$invest_return_7 <- cumprod(1+portfolio_7$vw_return)
setDT(portfolio_7)

portfolio_6 <- filter(stockdata,bin==6)
portfolio_6$tcapsum<-ave(portfolio_6$tcap_prev, portfolio_6$date, FUN=sum)

#calculate value weighted return for each period
portfolio_6<-portfolio_6 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

portfolio_6
setDT(portfolio_6)

portfolio_6$invest_return_6 <- cumprod(1+portfolio_6$vw_return)
setDT(portfolio_6)


portfolio_5 <- filter(stockdata,bin==5)
portfolio_5$tcapsum<-ave(portfolio_5$tcap_prev, portfolio_5$date, FUN=sum)

#calculate value weighted return for each period
portfolio_5<-portfolio_5 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

portfolio_5
setDT(portfolio_5)

portfolio_5$invest_return_5 <- cumprod(1+portfolio_5$vw_return)
setDT(portfolio_5)

portfolio_4 <- filter(stockdata,bin==4)
portfolio_4$tcapsum<-ave(portfolio_4$tcap_prev, portfolio_4$date, FUN=sum)

#calculate value weighted return for each period
portfolio_4<-portfolio_4 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

portfolio_4
setDT(portfolio_4)

portfolio_4$invest_return_4 <- cumprod(1+portfolio_4$vw_return)
setDT(portfolio_4)

portfolio_3 <- filter(stockdata,bin==3)
portfolio_3$tcapsum<-ave(portfolio_3$tcap_prev, portfolio_3$date, FUN=sum)

#calculate value weighted return for each period
portfolio_3<-portfolio_3 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

portfolio_3
setDT(portfolio_3)

portfolio_3$invest_return_3 <- cumprod(1+portfolio_3$vw_return)
setDT(portfolio_3)

portfolio_2 <- filter(stockdata,bin==2)
portfolio_2$tcapsum<-ave(portfolio_2$tcap_prev, portfolio_2$date, FUN=sum)

#calculate value weighted return for each period
portfolio_2<-portfolio_2 %>% group_by(date) %>%
  summarise(vw_return = sum(return * tcap_prev / tcapsum))

portfolio_2
setDT(portfolio_2)

portfolio_2$invest_return_2 <- cumprod(1+portfolio_2$vw_return)
setDT(portfolio_2)




portfolios<-merge(portfolios,portfolio_9[,.(date,invest_return_9)], by.x='date', by.y='date')
portfolios<-merge(portfolios,portfolio_8[,.(date,invest_return_8)], by.x='date', by.y='date')
portfolios<-merge(portfolios,portfolio_7[,.(date,invest_return_7)], by.x='date', by.y='date')
portfolios<-merge(portfolios,portfolio_6[,.(date,invest_return_6)], by.x='date', by.y='date')
portfolios<-merge(portfolios,portfolio_5[,.(date,invest_return_5)], by.x='date', by.y='date')
portfolios<-merge(portfolios,portfolio_4[,.(date,invest_return_4)], by.x='date', by.y='date')
portfolios<-merge(portfolios,portfolio_3[,.(date,invest_return_3)], by.x='date', by.y='date')
portfolios<-merge(portfolios,portfolio_2[,.(date,invest_return_2)], by.x='date', by.y='date')

portfolios
ggplot(data=portfolios,aes(x=date)) + geom_line(aes(y=invest_return_top,color="bin10"))+ 
                                      geom_line(aes(y=invest_return_bottom,color="bin01"))+ 
                                      #geom_line(aes(y=invest_return_wml,color="WML")) +
                                      #geom_line(aes(y=invest_rf,color="RF"))+
                                      geom_line(aes(y=invest_return_9,color="bin09"))+
                                      geom_line(aes(y=invest_return_8,color="bin08"))+
                                      geom_line(aes(y=invest_return_7,color="bin07"))+
                                      geom_line(aes(y=invest_return_6,color="bin06"))+
                                      geom_line(aes(y=invest_return_5,color="bin05"))+
                                      geom_line(aes(y=invest_return_4,color="bin04"))+
                                      geom_line(aes(y=invest_return_3,color="bin03"))+
                                      geom_line(aes(y=invest_return_2,color="bin02"))

check_data <- fread("./data/m_m_pt_tot.txt")
colnames(check_data)<- c('date','bin','return','tcap','v5')
check_data[,date:=as.Date(paste0(date),format="%Y%m%d")]
lapply(check_data,class)
check_data_plot <- filter(check_data, date<=max(portfolios$date)&date>="2001-09-28"&bin==10)
check_data_plot$invest_return_check_10 <- cumprod(1+check_data_plot$return) 
check_data_plot$return_1 <- filter(check_data,  date<=max(portfolios$date)&date>="2001-09-28"&bin==1)$return
check_data_plot$invest_return_check_1 <- cumprod(1+check_data_plot$return_1) 

ggplot(data=check_data_plot,aes(x=date)) + geom_line(aes(y=invest_return_check_10,color="bin10"))+
  geom_line(aes(y=invest_return_check_1,color="bin1"))


cor(portfolio_10$vw_return,check_data_plot$return)
cor(portfolio_1$vw_return,check_data_plot$return_1)

min(portfolios$date)
