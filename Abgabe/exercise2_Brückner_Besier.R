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

# load results of exercise 1
load("wksp/exercise1.RData")

# delete columns containing top and bottom returns plus existing RF column (will be added through the merge again)
portfolios<-portfolios[, !c("return_bottom", "return_top", "RF")]
portfolios
# read Fama-French data
ffdata<-fread("./data/F-F_Research_Data_Factors.csv")
ffdata

# convert date strings to actual dates
ffdata[,date:=as.Date(paste(year, month, "01", sep="-"))]
day(ffdata$date)<-days_in_month(ffdata$date)
ffdata

# delete date columns not needed anymore
ffdata<-ffdata[, !c("V1", "year", "month"), with=FALSE]
ffdata

setnames(ffdata,c("mktrf", "smb", "hml", "rf", "date"))
ffdata$rf<-ffdata$rf/100 # convert from percentage values to actual values
ffdata$mktrf<-ffdata$mktrf/100
ffdata$smb<-ffdata$smb/100
ffdata$hml<-ffdata$hml/100
ffdata


# merge ffdata with portfolio data
portfolios<-merge(portfolios, ffdata, by.x="date", by.y="date")
portfolios

# calculate excess return for the regression
portfolios$return_wml_exc<-portfolios$return_wml - portfolios$rf
portfolios

# regress the excess returns on the Fama-French factors
ffreg<-lm(return_wml_exc ~ mktrf + smb + hml, data=portfolios)
summary(ffreg)
portfolios

# calculate the beta of the WML portfolio using the standard CAPM beta formula
# for the formula see for example Fama, Eugene F. and French, Kenneth R.: The Capital Asset Pricing Model: Theory
# and Evidence. In: Journal of Economic Perspectives 18 (2004), Nr. 3, p. 25 - 46. 
beta = cov(portfolios$return_wml, portfolios$return_mkt)/var(portfolios$return_mkt)

# calculate the alpha for each date by rearranging the CAPM formula found in the paper given above
portfolios$alpha<-portfolios$return_wml - portfolios$rf - beta * (portfolios$return_mkt - portfolios$rf)

# plot the alphas
ggplot(data=portfolios,aes(x=date)) + geom_line(aes(y=alpha, color="alpha"),colour="#009682")+
  xlab("Date")+
  ylab("Alpha")

# calculate the mean alpha
mean(portfolios$alpha)
count(portfolios$alpha > 0)
count(portfolios$alpha < 0)
