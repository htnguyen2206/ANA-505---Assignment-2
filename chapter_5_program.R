# Analysis of Economic Time Series (R)

# economic time series gathered with this program are continually
# updated... so predictive models, forecasts, and data visualizations
# produced by this program may differ from those shown in the book

## Use library function to install quantmod (Quantitative Financial Modelling Framework)
library(quantmod) # use for gathering and charting economic data
## Use library function to install Lubridate to work with date and time
library(lubridate) # date functions
## Use library function to install latticeExtra to generate statistical graphics
library(latticeExtra) # package used for horizon plot
## Use library function to install forecast to forecast from time series or time series models
library(forecast) # functions for time series forecasting 
## Use library function to install lmtest to provide a collection of tests, data sets, and examples for diagnostic checking in linear regression models.
library(lmtest) # for Granger test of causality
## Use library function to install par to create 2x2 plotting window
par(mfrow = c(2,2)) # four plots on one window/page

## From line 24 to 44: load data from data file "Fred" ER -->draw chart of employment rate --> set date format as year month date for data time series (start month --> end month of data series)
# Economic Data from Federal Reserve Bank of St. Louis (FRED system)
# National Civilian Unemployment Rate (monthly, percentage)
## Load and manage data from data sources "FRED"
getSymbols("UNRATENSA", src="FRED", return.class = "xts")

ER <- 100 - UNRATENSA # convert to employment rate

# Use dimnames to query rows and columns in ER data file
dimnames(ER)[2] <- "ER"

## Use ChartSeries to create chart with theme = "white"
chartSeries(ER,theme="white")

##use as.data.frame to check if an object is a data frame, or coerce it if possible.
ER.data.frame <- as.data.frame(ER)

##making the date in format of year month date
ER.data.frame$date <- ymd(rownames(ER.data.frame))

##Use ts() to convert a numeric vector into an R time series object. It contains information of start month, end month and frequency of observation
ER.time.series <- ts(ER.data.frame$ER, 
  start = c(year(min(ER.data.frame$date)),month(min(ER.data.frame$date))),
  end = c(year(max(ER.data.frame$date)),month(max(ER.data.frame$date))),
  frequency=12)



## For line 50 - 60: the same as the chunk of code from line 24  to 44: load data from data file "Fed_ DGO" --> draw chart of DGO data --> set date format as year/month/date for time data in the source file
# Manufacturers' New Orders: Durable Goods (millions of dollars) 
getSymbols("DGORDER", src="FRED", return.class = "xts")
DGO <- DGORDER/1000 # convert to billions of dollars
dimnames(DGO)[2] <- "DGO" # use simple name for index
chartSeries(DGO, theme="white") 
DGO.data.frame <- as.data.frame(DGO)
DGO.data.frame$DGO <- DGO.data.frame$DGO
DGO.data.frame$date <- ymd(rownames(DGO.data.frame))
DGO.time.series <- ts(DGO.data.frame$DGO, 
  start = c(year(min(DGO.data.frame$date)),month(min(DGO.data.frame$date))),
  end = c(year(max(DGO.data.frame$date)),month(max(DGO.data.frame$date))),
  frequency=12)

## Line 64 - 74: load data from source file "FRED-ICS", draw chart of ICS data, format time data as year month date
# University of Michigan Index of Consumer Sentiment (1Q 1966 = 100)
getSymbols("UMCSENT", src="FRED", return.class = "xts")
ICS <- UMCSENT # use simple name for xts object
dimnames(ICS)[2] <- "ICS" # use simple name for index
chartSeries(ICS, theme="white")
ICS.data.frame <- as.data.frame(ICS)
ICS.data.frame$ICS <- ICS.data.frame$ICS
ICS.data.frame$date <- ymd(rownames(ICS.data.frame))
ICS.time.series <- ts(ICS.data.frame$ICS, 
  start = c(year(min(ICS.data.frame$date)), month(min(ICS.data.frame$date))),
  end = c(year(max(ICS.data.frame$date)),month(max(ICS.data.frame$date))),
  frequency=12)

## Line 78 - 88: Load data from source file "FRED_NHS", draw chart of NHS data, format time data as year month date
# New Homes Sold in the US, not seasonally adjusted (monthly, millions)
getSymbols("HSN1FNSA",src="FRED",return.class = "xts")
NHS <- HSN1FNSA
dimnames(NHS)[2] <- "NHS" # use simple name for index
chartSeries(NHS, theme="white")
NHS.data.frame <- as.data.frame(NHS)
NHS.data.frame$NHS <- NHS.data.frame$NHS
NHS.data.frame$date <- ymd(rownames(NHS.data.frame))
NHS.time.series <- ts(NHS.data.frame$NHS, 
  start = c(year(min(NHS.data.frame$date)),month(min(NHS.data.frame$date))),
  end = c(year(max(NHS.data.frame$date)),month(max(NHS.data.frame$date))),
  frequency=12)

# define multiple time series object
## combining column of ER, DGO, ICS, NHS time series above, then using dimnames to retrieve the data of the data combination of ER, DGO, ICS, NHS
economic.mts <- cbind(ER.time.series, DGO.time.series, ICS.time.series,
  NHS.time.series) 
  dimnames(economic.mts)[[2]] <- c("ER","DGO","ICS","NHS") # keep simple names 

## excluding the missing value in economic.mts data, and nameing the new data "modeling.mts".
modeling.mts <- na.omit(economic.mts) # keep overlapping time intervals only

# plot multiple time series 
## creating pdf region with height =11 and width = 8.5
pdf(file="fig_economic_analysis_mts_R.pdf",width = 8.5,height = 11)    
## ploting "modeling.mts" data 
plot(modeling.mts,main="")
dev.off()

# create new indexed series IER using base date March 1997
## calculating mean of data and assigning data a new data name
ER0 <- mean(as.numeric(window(ER.time.series,start=c(1997,3),end=c(1997,3))))
IER.time.series <- (ER.time.series/ER0) * 100  

# create new indexed series IDGO using base date March 1997
## calculating mean of data and assigning data a new data name
DGO0 <- mean(as.numeric(window(DGO.time.series,start=c(1997,3),end=c(1997,3))))
IDGO.time.series <- (DGO.time.series/DGO0) * 100  

# create new indexed series INHS using base date March 1997
## calculating mean of data and assinging data a new data name
NHS0 <- mean(as.numeric(window(NHS.time.series,start=c(1997,3),end=c(1997,3))))
INHS.time.series <- (NHS.time.series/NHS0) * 100  

# create a multiple time series object from the index series
## combing columns in new data above into a new data file, retrieve data, cleaning missing value in new data, then printing data
economic.mts <- cbind(IER.time.series,
IDGO.time.series,
ICS.time.series,
INHS.time.series) 
dimnames(economic.mts)[[2]] <- c("IER","IDGO","ICS","INHS")
working.economic.mts <- na.omit(economic.mts) # months complete for all series
# partial listing to check calculations
print(head(working.economic.mts))

# plot multiple economic time series as horizon plot
# using the index 100 as the reference point (origin = 100)
# with scaling fixed across the index numbers (horizonscale = 25)
# use ylab rather than strip.left, for readability
# also shade any times with missing data values.
# latticeExtra package used for horizon plot
##creating pdf region, then printing the pdf region using horizon plot
pdf(file="fig_economic_time_series_indexed_R.pdf",width = 8.5,height = 11)
print(horizonplot(working.economic.mts, colorkey = TRUE,
  layout = c(1,4), strip.left = FALSE, origin = 100, horizonscale = 25,
  ylab = list(rev(colnames(working.economic.mts)), rot = 0, cex = 0.7)) +
  layer_(panel.fill(col = "gray90"), panel.xblocks(..., col = "white")))
dev.off()
  
# return to the individual economic time series prior to indexing  
# functions from forecast package for time series forecasting 

# ARIMA model fit to the employment rate data
## searching to find the best arima model using constraints provided, then printing summary of data.
ER.auto.arima.fit <- auto.arima(ER.time.series, d=NA, D=NA, max.p=3, max.q=3,
  max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
  start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
  ic=c("aic"), stepwise=TRUE, trace=FALSE,
  approximation=FALSE, xreg=NULL,
  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
  allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(ER.auto.arima.fit))
# national employment rate two-year forecast (horizon h = 24 months) 
## forecasting using arima model
ER.forecast <- forecast.Arima(ER.auto.arima.fit, h=24, level=c(90), 
  fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot national employment rate time series with two-year forecast 
## setting the pdf region, then ploting data of ER.forecast
pdf(file = "fig_economic_analysis_er_forecast_R.pdf", width = 11, height = 8.5)
plot(ER.forecast,main="", ylab="Employment Rate (100 - Unemployment Rate)",
  xlab = "Time", las = 1, lwd = 1.5)
dev.off()

# ARIMA model fit to the manufacturers' durable goods orders
## searching to find the best arima model using constraints provided, then printing summary of data.
DGO.auto.arima.fit <- auto.arima(DGO.time.series, d=NA, D=NA, max.p=3, max.q=3,
  max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
  start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
  ic=c("aic"), stepwise=TRUE, trace=FALSE,
  approximation=FALSE, xreg=NULL,
  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
  allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(DGO.auto.arima.fit))
# durable goods orders two-year forecast (horizon h = 24 months) 
## forecasting using arima model
DGO.forecast <- forecast.Arima(DGO.auto.arima.fit, h=24, level=c(90), 
  fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot durable goods time series with two-year forecast 
## setting the pdf region, then ploting data of DGO.forecast
pdf(file = "fig_economic_analysis_dgo_forecast_R.pdf", width = 11, height = 8.5)
plot(DGO.forecast,main="", ylab="Durable Goods Orders (billions of dollars)",
  xlab = "Time", las = 1, lwd = 1.5)
dev.off()  

# ARIMA model fit to index of consumer sentiment
## from line 193 - 208: searching to find the best arima model using constraints provided, then printing summary of data, forecasting using arima model, then setting the pdf region, then ploting data of ICS.forecast
ICS.auto.arima.fit <- auto.arima(ICS.time.series, d=NA, D=NA, max.p=3, max.q=3,
  max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
  start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
  ic=c("aic"), stepwise=TRUE, trace=FALSE,
  approximation=FALSE, xreg=NULL,
  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
  allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(ICS.auto.arima.fit))
# index of consumer sentiment two-year forecast (horizon h = 24 months) 
ICS.forecast <- forecast.Arima(ICS.auto.arima.fit, h=24, level=c(90), 
  fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot index of consumer sentiment time series with two-year forecast 
pdf(file = "fig_economic_analysis_ics_forecast_R.pdf", width = 11, height = 8.5)
plot(ICS.forecast,main="", ylab="Index of Consumer Sentiment (1Q 1966 = 100)",
  xlab = "Time", las = 1, lwd = 1.5)
dev.off()

# ARIMA model fit to new home sales
## from line 212 - 227: searching to find the best arima model using constraints provided, then printing summary of data, forecasting using arima model, then setting the pdf region, then ploting data of NHS.forecast
NHS.auto.arima.fit <- auto.arima(NHS.time.series, d=NA, D=NA, max.p=3, max.q=3,
  max.P=2, max.Q=2, max.order=3, start.p=2, start.q=2,
  start.P=1, start.Q=1, stationary=FALSE, seasonal=TRUE,
  ic=c("aic"), stepwise=TRUE, trace=FALSE,
  approximation=FALSE, xreg=NULL,
  test=c("kpss","adf","pp"), seasonal.test=c("ocsb","ch"),
  allowdrift=FALSE, lambda=NULL, parallel=FALSE, num.cores=NULL)
print(summary(NHS.auto.arima.fit))
# new home sales two-year forecast (horizon h = 24 months) 
NHS.forecast <- forecast.Arima(NHS.auto.arima.fit, h=24, level=c(90), 
  fan=FALSE, xreg=NULL, bootstrap=FALSE)
# plot new home sales time series with two-year forecast 
pdf(file = "fig_economic_analysis_nhs_forecast_R.pdf", width = 11, height = 8.5)
plot(NHS.forecast,main="", ylab="New Homes Sold (millions)",
  xlab = "Time", las = 1, lwd = 1.5)
dev.off()

# Which regressors have potential as leading indicators?
# look for relationships across three of the time series
# using the period of overlap for those series
# function from lmtest package for Granger test of causality
## performing a test for Granger causality between ICS vs ER, ICS vs DGO, DGO vs ER, DGO vs ICS, ER vs DGO, ER vs ICS with the order of lag ~ 3
grangertest(ICS~ER, order = 3, data=modeling.mts)
grangertest(ICS~DGO, order = 3, data=modeling.mts)
grangertest(DGO~ER, order = 3, data=modeling.mts)
grangertest(DGO~ICS, order = 3, data=modeling.mts)
grangertest(ER~DGO, order = 3, data=modeling.mts)
grangertest(ER~ICS, order = 3, data=modeling.mts)

# export data frames for economic measures 
## save data file in csv format
write.csv(ER.data.frame, file = "FRED_ER_data.csv", row.names = FALSE)
write.csv(DGO.data.frame, file = "FRED_DGO_data.csv", row.names = FALSE)
write.csv(ICS.data.frame, file = "FRED_ICS_data.csv", row.names = FALSE)
write.csv(NHS.data.frame, file = "FRED_NHS_data.csv", row.names = FALSE)

# save current workspace
## save the file as image
save.image(file = "R_workspace.Rdata")

# Suggestions for the student:
# Explore additional forecasting methods such as exponential smoothing.
# Explore dynamic linear models and state space approaches.
# Gather data on additional economic measures that might be regarded
# as leading indicators. Select an industry to study, examine relevant 
# economic indicators and possible relationships to financial performance 
# of companies within that industry (stock prices or returns).

