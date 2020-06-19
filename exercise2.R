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

# read Fama-French data
ffdata<-fread("./data/F-F_Research_Data_Factors.csv")
ffdata$RF<-ffdata$RF/100 # convert from percentage values to actual values
ffdata[,date:=as.Date(paste(year, month, "01", sep="-"))]
day(ffdata$date)<-days_in_month(ffdata$date)
ffdata
ffdata<-ffdata[, !c("V1", "year", "month"), with=FALSE] # delete date columns not needed anymore
ffdata

# merge ffdata with portfolio data
portfolios$RF<-NULL # delete existing RF column (will be added through the merge again)
portfolios<-merge(portfolios, ffdata, by.x="date", by.y="date")
portfolios
