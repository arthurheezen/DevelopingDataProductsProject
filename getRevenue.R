#Install package
#  install.packages("rjson")
require(rjson)

# Set directory
setwd("C:\\Users\\Michelle\\Sync Arthur\\data\\DS9DevData\\Project")
JSONFile <- ".\\LARevenueData.json"

# Data source: LA City Revenue
# Human readable URL: 
#   https://controllerdata.lacity.org/Revenue/City-Revenue-by-Month/s234-w655
fileURL <- "https://controllerdata.lacity.org/api/views/s234-w655/rows.json?accessType=DOWNLOAD"

# Download data from website
download.file(fileURL,destfile = JSONFile)

#Extract JSON data using rjson
rev <- fromJSON(file=JSONFile)
datalength <- length(rev$data)
yeardata <- {}
monthdata <- {}
department <- {}
revenue <- {}
fiscalperiod <- {}

for (x in 1:datalength) {
  yeardata <- c(yeardata,as.integer(noquote(rev$data[[x]][9])))
  monthdata <- c(monthdata,toString(noquote(rev$data[[x]][10])))
  department <- c(department,toString(noquote(rev$data[[x]][12])))
  revenue <- c(revenue,as.double(noquote(rev$data[[x]][13])))
  fiscalperiod <- c(fiscalperiod,toString(noquote(rev$data[[x]][19]))) }

#Bind columns and convert it to dataframe
revdata <- as.data.frame(cbind(department, yeardata, monthdata, revenue,fiscalperiod))
revdata[,4] <- as.double(revenue)
head(revdata)

# install.packages("plyr")
library(plyr)
revtotal <- ddply(revdata,.(fiscalperiod,monthdata), summarize, monthly_revenue = sum(revenue))
head(revtotal)

# install.packages('forecast')
library(forecast)
mts <- ts(revtotal$monthly_revenue, start=c(2012,1),end=c(2016,8),frequency=12)
fit <- stl(mts, s.window="period", robust = TRUE)
op <- par(mar = c(0, 4, 0, 3), oma = c(5, 0, 4, 0), mfcol = c(4, 1))

plot(fit, set.pars = NULL, labels  =  NULL,
     main = "City of Los Angeles - Revenue in Dollars\nSeasonal decompositon of Time series by Loess (STL)")
(iO <- which(fit$weights  < 1e-8))
sts <- fit$time.series
points(time(sts)[iO], 0.8* sts[,"remainder"][iO], pch = 4, col = "red")
par(op)   # reset
