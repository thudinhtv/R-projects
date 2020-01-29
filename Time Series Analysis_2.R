#Set up working directory
workingdirectory = "Path\\TimeSeriesData"
setwd(workingdirectory)

#Read in libraries
options(digits = 4)
library(tseries)
library(forecast)
library(lmtest)

#Read in data
global_data = read.table('global.txt', sep = '\t', header = TRUE)
class(global_data)
summary(global_data)

#Converting dataframe to time series object
global_ts = ts(global_data, frequency = 12, start = c(1856, 1))
class(global_ts)
head(global_ts, n=24)


#1. Determine whether or not your data is stationary
#a. Subjective assessment: plot the data to see if there is any trend
global_ts_dc = decompose(global_ts)
plot(global_ts_dc$trend, main="Global Temperature Trend")

#b. Objective assessment
#b1. Use regression to assess stationarity by regressing trend onto time
global_trendcomp = global_ts_dc$trend
global_trend_data = data.frame(trend = c(global_trendcomp),
                               time = c(time(global_trendcomp)))

global_trend_reg = lm(global_trend_data$trend ~ global_trend_data$time)
summary(global_trend_reg)
## Result: p-value highly significant -> the trend component heavily influences the time series

## Compare the means of the time series with and without the trend
### Remove season; assess mean for trend data
global_ts_trend = global_ts - global_ts_dc$seasonal
mean(global_ts_trend)
var(global_ts_trend)

### Remove trend and season; assess mean for data without trend
global_ts_rand = global_ts - global_ts_dc$seasonal - global_ts_dc$trend
mean(na.omit(global_ts_rand))
var(na.omit(global_ts_rand))

### Results: mean and variance of trend data are higher than those of data w/o trend
### -> The trend component does increase both the mean and variance of Global data

#b2.Augmented Dickey–Fuller (ADF) t-test
adf.test(global_ts_trend, k = 20, alternative = "stationary")
## Result: significant, indicating stationarity

#b3. Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(global_ts_trend)
## Result: Significant, indicating non-stationarity


#c. If it is not stationary, how many times must you difference it to obtain stationarity
global_ts_diff1 = diff(global_ts_trend, differences = 1)
plot(global_ts_diff1, main = 'Global Temperature Without Seasonal or Trend via Differencing')

adf.test(global_ts_diff1, k = 20, alternative = "stationary")
## Result: significant, indicating stationarity

kpss.test(global_ts_diff1)
## Result: non-significant, indicating stationarity


#2. Propose at least three different models for your data and justify your decision
## Autocorrelation plot
acf(global_ts_diff1, lag.max = 20, main='ACF Plot of Global Temperature')
pacf(global_ts_diff1, lag.max = 20, main='PACF Plot of Global Temperature')

## ARIMA(AR, I/Diff, MA) = ARIMA(p,d,q)
## Difference = 1 -> d=1

## ARMA(1,1): The correlogram “dies down” while the partial correlogram also 
## dies down with multiple effects after the first lag -> ARIMA(1, 1, 1)

## ARMA(1,0): Since the partial correlogram dies off dramatically after the first lag,
## then the effects can be attributed to autocorrelation effects -> ARIMA(1, 1, 1)

## ARMA(2,0): ARIMA(2,1,0) model.

## ARMA(2,1): ARIMA(2,1,1) model


#3. Estimate the components of your models and perform diagnostics
global_arima1 = arima(global_ts_diff1, order = c(1, 0, 1), method = "ML")
global_arima1
## Result: AIC -2381

global_arima2 = arima(global_ts_diff1, order = c(1, 0, 0), method = "ML")
global_arima2
## Result: AIC -2181 -> lowest AIC -> best

global_arima3 = arima(global_ts_diff1, order = c(2, 0, 0), method = "ML")
global_arima3
## Result: AIC -2250

global_arima4 = arima(global_ts_diff1, order = c(2, 0, 1), method = "ML")
global_arima4
## Result: AIC -2403 -> best (lowest)

coeftest(global_arima4)

# Obtain BIC for Models
global_arima1_bic = AIC(global_arima1, k = log(length(global_ts_diff1)))
global_arima2_bic = AIC(global_arima2, k = log(length(global_ts_diff1)))
global_arima3_bic = AIC(global_arima3, k = log(length(global_ts_diff1)))
global_arima4_bic = AIC(global_arima4, k = log(length(global_ts_diff1)))

global_arima1_bic
## Result: -2359

global_arima2_bic
## Result: -2164 

global_arima3_bic
## Result: -2228

global_arima4_bic
## Result: -2376 -> best (lowest)


#4. Forecast your models and assess each one
# ARMA(1,1)
global_arima1_fore = forecast(global_arima1, h = 20)
accuracy(global_arima1_fore)

# ARMA(1,0)
global_arima2_fore = forecast(global_arima2, h = 20)
accuracy(global_arima2_fore)

# ARMA(2,0)
global_arima3_fore = forecast(global_arima3, h = 20)
accuracy(global_arima3_fore)

# ARMA(2,1)
global_arima4_fore = forecast(global_arima4, h = 20)
accuracy(global_arima4_fore)
