# 1. Import the "Advance Retail Sales" dataset from FRED (FRED code is RSXFSN)
# Clearing the work
rm(list = ls())
graphics.off()
# Read the csv data file
ARS=read.csv("SALES.csv",header = TRUE)
ARS
class(ARS)
colnames(ARS) <- c("DATE","SALES")
ARS
head(ARS)
summary(ARS)

# Changing it to time series
SALES=ts(ARS$SALES,start = c(1992,1),frequency = 12)
DATE=ts(ARS$DATE,start = c(1992,1),frequency = 12)
class(ARS)
plot(SALES)

# 1.a. Fit linear model
m1=lm(SALES~time(SALES)) # plot is not stationary, changing overtime and not identical. The variance is changing too
plot(SALES)
abline(m1,col="green")
cycle(SALES)
plot(aggregate(SALES,FUN = mean))
boxplot(SALES~cycle(SALES))

install.packages("xts")
install.packages("tseries")
library(tseries)
library(xts)
adf.test(SALES,k=12)
plot(SALES) #although through adf.test, it says "stationary", when we look at the plot of the SALES, it is actually NOT

# 1.b. Applying ARIMA model to make it stationary
# Making it stationary
plot(SALES)
plot(log(SALES))
plot(diff(log(SALES)))
Our_New_Data=diff(log(SALES))
# Now it becomes stationary
acf(Our_New_Data) #from here we found Q=1
pacf(Our_New_Data) #from here we found P=0
# Applying ARIMA
Our_New_Data=log(SALES)
The_Arima=arima(Our_New_Data,order=c(0,1,1),seasonal = list(order=c(0,1,1),period=12))
The_Arima

# 1.c. Predicting the next 20 years
Predictions=predict(The_Arima,n.ahead=20*12)
Predictions
PRED=Predictions$pred
PRED=exp(PRED)
ts.plot(SALES,PRED,lty=c(1,3))

###############################################################################
# 2. Determining the relationship between the financial cycle and business cycle of South African business cycle
# Clearing the work
rm(list = ls())
graphics.off()
# Importing the data
library(quantmod)
#install.packages("vars")
library(vars)
#import the data into R
getSymbols(c("GDPCTPI", "UNRATE", "A939RX0Q048SBEA", "USSTHPI"),
           src = "FRED")
#"UNRATE" : Unemployment.
#"GDPCTPI" :Gross Domestic Product: Chain-type Price Index (gdp_deflator).
#"A939RX0Q048SBEA" : gdp
#"USSTHPI" : house price

# Ordering of variables matters for impulse
# Response functions
macro_data = merge(GDPCTPI, UNRATE, A939RX0Q048SBEA, USSTHPI)
head(macro_data)

#Delete Missing Values
macro_data=na.omit(macro_data)
head(macro_data)
#renaming the columns of the data frame
colnames(macro_data) = c("gdp_deflator","unemployment","gdp","house_price")

#Taking the rates of change in GDP and GDP deflators as we did before 
#Delt is for percentage change , and 400 is 4(qurters) and 100 (percentage) 
macro_data$gdp_deflator = 400 * Delt(macro_data$gdp_deflator,type = "log")
macro_data$gdp = 400 * Delt(macro_data$gdp, typ = "log")
macro_data = na.omit(macro_data)

#Taking only the values that are after January 1976
macro_data = macro_data[index(macro_data) >= "1976-01-01", ]
head(macro_data)

#Before fitting a VAR model, let's explore 
#Changing them to time sereies 
GDP=ts(macro_data$gdp,start = c(1976,1),frequency = 4)
DEFLATOR=ts(macro_data$gdp_deflator,start = c(1976,1),frequency = 4)
UNEMPLOYMENT=ts(macro_data$unemployment,start = c(1976,1),frequency = 4)
HOUSEPRICE=ts(macro_data$house_price,start = c(1976,1),frequency = 4)
par(mfrow=c(1,1),mar=c(2.2,2.2,1,1))
ts.plot(cbind(GDP,DEFLATOR,UNEMPLOYMENT,HOUSEPRICE),col=c("red","blue","green","black"),xlim=c(1976,2021),ylim=c(-500,500))
grid()
legend("bottomright",legend = c("GDP","DEFLATOR","UNEMP","HOUSEPRICE"),col = c("red","blue","green","black"),bty = "n",lty = 1)
plot(macro_data)
class(macro_data)

# Detrending data with a linear filter
lin.mod <- lm(GDP ~ time(GDP))
lin.trend <- lin.mod$fitted.values  # fitted values pertain to time trend
linear <- ts(lin.trend, start = c(1976, 1), frequency = 4)  # create a time series variable for trend
lin.cycle <- GDP - linear  # cycle is the difference between the data and linear trend

# plotting this result with the aid of the following commands, where the trend and the cycle are plotted on separate figures
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)  # plot two graphs side by side and squash margins
plot.ts(GDP, ylab = "")  # first plot time series
lines(linear, col = "red")  # include lines over the plot
legend("topleft", legend = c("data", "trend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(lin.cycle, ylab = "")  # second plot for cycle
legend("topright", legend = c("cycle    "), lty = 1, col = c("black"), 
       bty = "n")

# Installing devtools
devtools::install_github("KevinKotze/tsm")
install.packages("mFilter", repos = "https://cran.rstudio.com/", 
                 dependencies = TRUE)

# Accessing the libraries
install.packages("devtools")
install_github("KevinKotze/tsm")
install.packages("mFilter")
library(devtools)
library(tsm)
library(mFilter)

# Detrending data with the Hodrick-Prescott filter
hp.decom <- hpfilter(GDP, freq = 1600, type = "lambda")
par(mfrow = c(1, 2), mar = c(2.2, 2.2, 1, 1), cex = 0.8)
plot.ts(GDP, ylab = "")  # plot time series
lines(hp.decom$trend, col = "red")  # include HP trend
legend("topleft", legend = c("data", "HPtrend"), lty = 1, 
       col = c("black", "red"), bty = "n")
plot.ts(hp.decom$cycle, ylab = "")  # second plot for plot cycle
legend("topleft", legend = c("HPcycle"), lty = 1, col = c("black"), 
       bty = "n")

# Finding the trend
hp.decom$trend

# Forcasting the trend
library(forecast)
forecast(hp.decom)
class(hp.decom)
plot(hp.decom$cycle)
hp.decom

###############################################################################

# 3.a. Fitting a VAR model
# Clearing up the work
rm(list = ls())
graphics.off()

# Importing the data
mydata=read.csv("SampleVAR.csv")
head(mydata)

# Changing it to time series
GDP=ts(mydata$real_gdp_growth, start = c(1999,1),frequency = 4)
UNEMPLOYMENT=ts(mydata$unem, start = c(1999,1),frequency = 4)
DATE=ts(mydata$date,start = c(1999,1),frequency = 4)

# Getting the ts plot
ts.plot(cbind(GDP,UNEMPLOYMENT),col=c(2,3,4))
legend("topright",legend = c("GDP Growth","UNEMPLOYMENT"),col = c("2","3"),lty = 1,bty = "n")

# Creating the macro data set
macro_data=subset(mydata,select = c(real_gdp_growth,unem))
colnames(macro_data)=c("GDP_Growth","UNEMPLOYMENT")
macro_data

#Doing the VAR -select to choose the appropriate number of lags
VARselect(macro_data) # smallest AIC is 4
macro_var=VAR(macro_data,p=4)
summary(macro_var)

# FInding the lag
acf(GDP)
acf(UNEMPLOYMENT)
# We obtained log=4

# Stationarising
plot(log(GDP))
plot(log(UNEMPLOYMENT))
plot(diff(log(GDP)))
plot(diff(log(UNEMPLOYMENT)))
Log_GDP=diff(log(GDP))
Log_UNEMPLOYMENT=diff(log(UNEMPLOYMENT))
acf(Log_GDP)
acf(Log_UNEMPLOYMENT)
# We also obtained log=4

# Ljung Box test of serial correlation (I think it is mentioned before) among residuals
# If P>0.05, Residuals are not autocorrelated up to chosen lag
Box.test(macro_var$varresult$GDP_Growth$residuals,lag = 4)
Box.test(macro_var$varresult$UNEMPLOYMENT$residuals,lag = 4)

#  3.b. Ordering of variables matters for impulse
plot(irf(macro_var, impulse = "UNEMPLOYMENT",
         boot = 1000, ortho = F))

# 3.c. Forecasting GDP and Unemployment rate for 6 quarters ahead
forecast_macro <- predict(macro_var, n.ahead = 6)
plot(forecast_macro)
forecast_macro

