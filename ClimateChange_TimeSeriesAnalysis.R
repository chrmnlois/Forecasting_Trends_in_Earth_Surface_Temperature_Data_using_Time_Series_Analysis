# Import Libraries
library(forecast)
library(tseries)

# Set Working Directory
setwd("C:/Users/User/OneDrive/Documents/ClimateChangeAnalysis") # varies; based on file location

# Read Data Set
climate <- read.csv("climate.csv")

# Convert Date Column to a Proper Date Format
climate$dt <- as.Date(climate$dt)

# Drop Rows with Null Values
climate <- na.omit(climate)

# Create Time Series Object (Year 1980 - 2020)
ts_climate <- ts(climate$AverageTemperature, start = c(1980, 1), end = c(2013, 8), frequency = 1)

# Check if ts_climate is Stationary
plot(ts_climate) # check the trend
adf.test(ts_climate) # if p-value < 0.05 = stationary (qualified for time series analysis)

# Fit ARIMA Model
climate_model <- auto.arima(ts_climate, ic="aic", trace=TRUE) # results to best ARIMA model

# Forecast Average Global Land Temperature for the next 30 years
climate_forecast <- forecast(climate_model, level = c(95), h=30)
climate_forecast # range of ave global land temperature in the next 30 years (Lo 95 & Hi 95)
plot(climate_forecast, xlab = "Years", ylab = "Average Temperature (in °C)", col="red", 
     main="Average Global Land Temperature for the next 30 years")

# Validate Forecast with Box.test (if p-value > 0.05, forecast is good)
Box.test(climate_forecast$resid, lag=5, type= "Ljung-Box") 
Box.test(climate_forecast$resid, lag=10, type= "Ljung-Box")
Box.test(climate_forecast$resid, lag=15, type= "Ljung-Box")
Box.test(climate_forecast$resid, lag=20, type= "Ljung-Box")

