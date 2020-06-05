# load the data.table package
library(data.table)

# read file with stock data
stockdata<-fread("./data/stock_data.csv")
stockdata

# implement a monthly momentum portfolio (daily version described in the paper would require information on daily 
# stock returns which we don't have)

