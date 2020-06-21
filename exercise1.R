#install.packages("data.table", "lubridate", "dplyr", ggplot2", "matrixStats")
# load the data.table package
library(data.table)
# load the lubridate package
library(lubridate)
# load the dplyr package to enable use of the pipe operator
library(dplyr)
# load the ggplot library to enable plotting
library(ggplot2)
# load the matrixStats library to enable use of the rowProds operation
library(matrixStats)
# read file with stock data
stockdata<-fread("./data/stock_data.csv")
#!!!for development purposes only: reduce size of the dataset
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
stockdata$available_returns<-rowSums(!is.na(stockdata[,return_cols, with=FALSE])) # compute no of available returns
stockdata

stockdata2<-NULL

stockdata$cum_return<-rowProds(1+as.matrix(stockdata[,return_cols, with=FALSE]), na.rm=T) # compute cumulative product
stockdata
stockdata<-stockdata[, !return_cols, with=FALSE] # drop return_cols (not needed anymore)
return_cols<-NULL
stockdata

# compute market return for each date (will be needed for exercise 2)
stockdata2<-stockdata
stockdata2<-stockdata2[,.(permno, date, return, tcap_shifted)]
stockdata2
stockdata2$tcapsum<-ave(stockdata2$tcap_shifted, stockdata2$date, FUN=sum)
stockdata2
stockdata2<-stockdata2 %>% group_by(date) %>%
    summarise(return_mkt = sum(return * tcap_shifted / tcapsum))
stockdata2

# convert back from a tibble to a data.table
setDT(stockdata2)
stockdata2<-stockdata2[complete.cases(stockdata2), ]
stockdata2

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
portfolios<-merge(portfolios, stockdata2, by.x="date", by.y="date")
stockdata2<-NULL
portfolios

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

#plotting
portfolios$cum_bottom<-cumprod(1+portfolios$return_bottom)
portfolios$cum_top<-cumprod(1+portfolios$return_top)
portfolios$cum_wml<-cumprod(1+portfolios$return_wml)
portfolios$cum_mkt<-cumprod(1+portfolios$return_mkt)
portfolios$cum_rf<-cumprod(1+portfolios$RF)

portfolios_plot<-portfolios[, c("date","cum_bottom", "cum_top", "cum_wml", "cum_mkt","cum_rf"), with=FALSE]

min_plot_date <- min(portfolios_plot$date)%m-% days(1)
portfolios_plot<-rbind(portfolios_plot,t(c(1,1,1,1,1,1))) 


ggplot(data=portfolios_plot,aes(x=date)) + 
    geom_line(aes(y=cum_bottom, color="Past Losers",linetype="Past Losers")) + 
    geom_line(aes(y=cum_top, color="Past Winners",linetype="Past Winners")) + 
    geom_line(aes(y=cum_wml, color="WML",linetype="WML")) + 
    geom_line(aes(y=cum_mkt,color="Market",linetype="Market"))+
    geom_line(aes(y=cum_rf,color="Riskfree",linetype="Riskfree"))+
    scale_color_manual("Color",values = c("Past Losers"="black","Past Winners"= "black",
                                  "WML"="#009682","Market"="#4664AA","Riskfree"="grey"))+
    scale_linetype_manual("Lines",values = c("Past Losers"="dashed","Past Winners"= "dotted",
                                     "WML"="solid","Market"="solid","Riskfree"="solid"))+
    xlab("Date")+
    ylab("Dollar value of investment")+
    guides(linetype=guide_legend(keywidth = 3, keyheight = 1),
         colour=guide_legend(keywidth = 3, keyheight = 1))



# delete columns used for plotting
portfolios<-portfolios[, !c("cum_bottom", "cum_top", "cum_wml", "cum_mkt","cum_rf"), with=FALSE]
# save results
save.image("wksp/exercise1.RData")

