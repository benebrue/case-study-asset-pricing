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

# delete columns containing top and bottom returns plus existing RF column
portfolios<-portfolios[, !c("return_bottom", "return_top", "RF")]
portfolios

# load recession indicator data
recessiondata<-fread("./data/nber_recessions.csv")
recessiondata
# check classes of columns
lapply(recessiondata, class)
# date is currently a char value --> convert date to an actual date
recessiondata[,date:=as.Date(date,format="%Y-%m-%d")]
lapply(recessiondata, class)

# opposed to the data description, the first day instead of the last day is specified for each date --> change this
day(recessiondata$date)<-days_in_month(recessiondata$date) # set day of the date = last day of the month
recessiondata

# merge recessiondata with the portfolio data
portfolios<-merge(portfolios, recessiondata, by.x="date", by.y="date")
portfolios

mean_wml<-mean(portfolios$return_wml)
median_wml<-median(portfolios$return_wml)

portfolios$wml_vs_mean<-ifelse(portfolios$return_wml > mean_wml, 1, 0)
portfolios$wml_vs_median<-ifelse(portfolios$return_wml > median_wml, 1, 0)
portfolios$wml_vs_zero<-ifelse(portfolios$return_wml > 0, 1, 0)
portfolios
cor(portfolios$recession, portfolios$wml_vs_mean)
cor(portfolios$recession, portfolios$wml_vs_median)
cor(portfolios$recession, portfolios$wml_vs_zero)

reg<-lm(return_wml ~ recession, data=portfolios)
summary(reg)
