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
portfolios_plot <- portfolios<-merge(portfolios_plot, recessiondata, by.x="date", by.y="date")

#just to visualize bear market
portfolios_plot$bear_market<-portfolios_plot$recession*6.5

#plot for exercise 4
plot_4 <- ggplot(data=portfolios_plot,aes(x=date)) + 
            geom_area(aes(y=bear_market,color="Bear-Market"),fill=rgb(red = 1, green = 0, blue = 0, alpha = 0.5))+
            geom_line(aes(y=cum_wml, color="WML"))+
            scale_color_manual("Legend",values = c("WML"="#009682","Bear-Market"=rgb(red = 1, green = 0, blue = 0, alpha = 0.0)))+
            xlab("Date")+
            ylab("Dollar value of investment")+
            ylim(0,6.5)
plot_4 + theme(legend.position = "none")

