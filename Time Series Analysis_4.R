#Set up working directory
workingdirectory = "Path\\TimeSeriesData"
setwd(workingdirectory)

#Read in libraries
options(digits = 4)
library(tseries)
library(forecast)
library(lmtest)

#Read in data
HospitalFinancial_data = read.table('CaliforniaHospital_FinancialData.txt', sep = '\t', header = TRUE)
str(HospitalFinancial_data)

#Subset the NonOp_Rev column to new dataset & Convert dataframe to time series object
NonOpRev_data = subset(HospitalFinancial_data, select = c("NONOP_REV"))
NonOpRev_ts = ts(NonOpRev_data, frequency = 4, start = c(2007, 1))
class(NonOpRev_ts)

# Autocorrelation Function with no seasonal or trend effects
NonOpRev_ts_dc = decompose(NonOpRev_ts)
NonOpRev_ts_trend = NonOpRev_ts - NonOpRev_ts_dc$seasonal

NonOpRev_ts_diff1 = diff(NonOpRev_ts_trend, differences = 1)
plot(NonOpRev_ts_diff1)

adf.test(NonOpRev_ts_diff1, k = 0, alternative = "stationary")
kpss.test(NonOpRev_ts_diff1)
## Result: ADF: Stationary; KPSS: Stationary

# Propose 2 potential ARMA models. 
acf(NonOpRev_ts_diff1, lag.max = 20)
pacf(NonOpRev_ts_diff1, lag.max = 20)

## ARMA(1,1)
NonOpRev_arima1 = arima(NonOpRev_ts_diff1, order = c(1, 0, 1), method = "ML")
NonOpRev_arima1

## ARMA(0,1)
NonOpRev_arima2 = arima(NonOpRev_ts_diff1, order = c(0, 0, 1), method = "ML")
NonOpRev_arima2

## BIC
NonOpRev_arima1_bic = AIC(NonOpRev_arima1, k = log(length(NonOpRev_ts_diff1)))
NonOpRev_arima2_bic = AIC(NonOpRev_arima2, k = log(length(NonOpRev_ts_diff1)))
NonOpRev_arima1_bic
NonOpRev_arima2_bic


## Forecast Errors
NonOpRev_arima1_fore = forecast(NonOpRev_arima1, h = 20)
NonOpRev_arima2_fore = forecast(NonOpRev_arima2, h = 20)
accuracy(NonOpRev_arima1_fore)
accuracy(NonOpRev_arima2_fore)

