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

#=================================================
#A.NonOp_Rev
#=================================================

#Subset the NonOp_Rev column to new dataset & Convert dataframe to time series object
NonOpRev_data = subset(HospitalFinancial_data, select = c("NONOP_REV"))
NonOpRev_ts = ts(NonOpRev_data, frequency = 4, start = c(2007, 1))
class(NonOpRev_ts)
NonOpRev_ts

#1.Determine if the variable is stationary
NonOpRev_ts_dc = decompose(NonOpRev_ts)
plot(NonOpRev_ts_dc)

## Regression
NonOpRev_trendcomp = NonOpRev_ts_dc$trend
NonOpRev_trend_data = data.frame(trend = c(NonOpRev_trendcomp), time = c(time(NonOpRev_trendcomp)))
NonOpRev_trend_reg = lm(NonOpRev_trend_data$trend ~ NonOpRev_trend_data$time)
summary(NonOpRev_trend_reg)

## Augmented Dickey–Fuller (ADF) Test (set k=0)
NonOpRev_ts_trend = NonOpRev_ts - NonOpRev_ts_dc$seasonal
adf.test(NonOpRev_ts_trend, k = 0, alternative = "stationary")

## Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
kpss.test(NonOpRev_ts_trend)

#2.Using autocorrelation function, determine if the data has seasonal and
# trend components. Please use a plot of the ACF to justify your conclusion

## Autocorrelation Function with seasonal and trend effects
acf(NonOpRev_ts)

adf.test(NonOpRev_ts, k = 0, alternative = "stationary")
kpss.test(NonOpRev_ts)

## Autocorrelation Function with trend effects (model without the seasonal component)
acf(NonOpRev_ts_trend, lag.max = 20)
plot(NonOpRev_ts_trend, main = 'Non-Operating Revenue Data without Seasonal Component')

#3.If needed, difference your variables and assess each one with the ADF & KPSS
# to determine stationariy. How many, if at all, differences, did you apply?

## Autocorrelation Function with no seasonal or trend effects
NonOpRev_ts_diff1 = diff(NonOpRev_ts_trend, differences = 1)
plot(NonOpRev_ts_diff1)
adf.test(NonOpRev_ts_diff1, k = 0, alternative = "stationary")
kpss.test(NonOpRev_ts_diff1)
### Result: ADF: Stationary; KPSS: Stationary

#4. For each variable, propose 3 alternatives ARMA models. 
# Provide estimates for each model
acf(NonOpRev_ts_diff1, lag.max = 20)
pacf(NonOpRev_ts_diff1, lag.max = 20)

## ARMA(1,1)
NonOpRev_arima1 = arima(NonOpRev_ts_diff1, order = c(1, 0, 1), method = "ML")
NonOpRev_arima1

## ARMA(0,1)
NonOpRev_arima2 = arima(NonOpRev_ts_diff1, order = c(0, 0, 1), method = "ML")
NonOpRev_arima2

## ARMA(4,1)
NonOpRev_arima3 = arima(NonOpRev_ts_diff1, order = c(4, 0, 1), method = "ML")
NonOpRev_arima3

coeftest(NonOpRev_arima2)


#5. Select the best fitting model for each of your variables 
# and include goodness of fit values
## Obtain BIC for Models
NonOpRev_arima1_bic = AIC(NonOpRev_arima1, k = log(length(NonOpRev_ts_diff1)))
NonOpRev_arima2_bic = AIC(NonOpRev_arima2, k = log(length(NonOpRev_ts_diff1)))
NonOpRev_arima3_bic = AIC(NonOpRev_arima3, k = log(length(NonOpRev_ts_diff1)))

NonOpRev_arima1_bic
NonOpRev_arima2_bic
NonOpRev_arima3_bic

## Forecast Errors
NonOpRev_arima1_fore = forecast(NonOpRev_arima1, h = 20)
NonOpRev_arima2_fore = forecast(NonOpRev_arima2, h = 20)
NonOpRev_arima3_fore = forecast(NonOpRev_arima3, h = 20)
accuracy(NonOpRev_arima1_fore)
accuracy(NonOpRev_arima2_fore)
accuracy(NonOpRev_arima3_fore)



#=================================================
#B.TOT_OP_EXP
#=================================================

#Subset the TOT_OP_EXP column to new dataset & Convert dataframe to time series object
TotOpExp_data = subset(HospitalFinancial_data, select = c("TOT_OP_EXP"))
TotOpExp_ts = ts(TotOpExp_data, frequency = 4, start = c(2007, 1))
class(TotOpExp_ts)
TotOpExp_ts

#1. Determine if the variable is stationary
TotOpExp_ts_dc = decompose(TotOpExp_ts)
plot(TotOpExp_ts_dc)

## Regression
TotOpExp_trendcomp = TotOpExp_ts_dc$trend
TotOpExp_trend_data = data.frame(trend = c(TotOpExp_trendcomp), time = c(time(TotOpExp_trendcomp)))
TotOpExp_trend_reg = lm(TotOpExp_trend_data$trend ~ TotOpExp_trend_data$time)

summary(TotOpExp_trend_reg)

## Augmented Dickey–Fuller (ADF) Test (set k=0)
TotOpExp_ts_trend = TotOpExp_ts - TotOpExp_ts_dc$seasonal
adf.test(TotOpExp_ts_trend, k = 0, alternative = "stationary")

## Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
kpss.test(TotOpExp_ts_trend)

#2. Using autocorrelation function, determine if the data has seasonal and trend components.
# Please use a plot of the ACF to justify your conclusion
## Autocorrelation Function with seasonal and trend effects
acf(TotOpExp_ts)

adf.test(TotOpExp_ts, k = 0, alternative = "stationary")
kpss.test(TotOpExp_ts)

## Autocorrelation Function with trend effects (model without the seasonal component)
acf(TotOpExp_ts_trend, lag.max = 20)
plot(TotOpExp_ts_trend, main = 'Total Operating Expense Data without Seasonal Component')

#3. If needed, difference your variables and assess each one with the ADF & KPSS
# to determine stationariy. How many, if at all, differences, did you apply?
## Autocorrelation Function with no seasonal or trend effects
TotOpExp_ts_diff1 = diff(TotOpExp_ts_trend, differences = 1)
plot(TotOpExp_ts_diff1)
adf.test(TotOpExp_ts_diff1, k = 0, alternative = "stationary")
kpss.test(TotOpExp_ts_diff1)
### Result: ADF: Stationary; KPSS: Stationary

#4. For each variable, propose 3 alternatives ARMA models. 
## Provide estimates for each model
acf(TotOpExp_ts_diff1, lag.max = 20)
pacf(TotOpExp_ts_diff1, lag.max = 20)

## ARMA(1,1)
TotOpExp_arima1 = arima(TotOpExp_ts_diff1, order = c(1, 0, 1), method = "ML")
TotOpExp_arima1

## ARMA(9,0)
TotOpExp_arima2 = arima(TotOpExp_ts_diff1, order = c(9, 0, 0), method = "ML")
TotOpExp_arima2

## ARMA(9,1)
TotOpExp_arima3 = arima(TotOpExp_ts_diff1, order = c(9, 0, 1), method = "ML")
TotOpExp_arima3

coeftest(TotOpExp_arima1)


#5. Select the best fitting model for each of your variables 
# and include goodness of fit values
## Obtain BIC for Models
TotOpExp_arima1_bic = AIC(TotOpExp_arima1, k = log(length(TotOpExp_ts_diff1)))
TotOpExp_arima2_bic = AIC(TotOpExp_arima2, k = log(length(TotOpExp_ts_diff1)))
TotOpExp_arima3_bic = AIC(TotOpExp_arima3, k = log(length(TotOpExp_ts_diff1)))

TotOpExp_arima1_bic
TotOpExp_arima2_bic
TotOpExp_arima3_bic

## Forecast Errors
TotOpExp_arima1_fore = forecast(TotOpExp_arima1, h = 20)
TotOpExp_arima2_fore = forecast(TotOpExp_arima2, h = 20)
TotOpExp_arima3_fore = forecast(TotOpExp_arima3, h = 20)
accuracy(TotOpExp_arima1_fore)
accuracy(TotOpExp_arima2_fore)
accuracy(TotOpExp_arima3_fore)



#=================================================
#C.NET_TOT
#=================================================

#Subset the NET_TOT column to new dataset & Convert dataframe to time series object
NetTot_data = subset(HospitalFinancial_data, select = c("NET_TOT"))
NetTot_ts = ts(NetTot_data, frequency = 4, start = c(2007, 1))
class(NetTot_ts)
NetTot_ts

#1. Determine if the variable is stationary
NetTot_ts_dc = decompose(NetTot_ts)
plot(NetTot_ts_dc)

## Regression
NetTot_trendcomp = NetTot_ts_dc$trend
NetTot_trend_data = data.frame(trend = c(NetTot_trendcomp), time = c(time(NetTot_trendcomp)))
NetTot_trend_reg = lm(NetTot_trend_data$trend ~ NetTot_trend_data$time)

summary(NetTot_trend_reg)

## Augmented Dickey–Fuller (ADF) Test (set k=0)
NetTot_ts_trend = NetTot_ts - NetTot_ts_dc$seasonal
adf.test(NetTot_ts_trend, k = 0, alternative = "stationary")

## Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Test
kpss.test(NetTot_ts_trend)

#2. Using autocorrelation function, determine if the data has seasonal and trend components.
# Please use a plot of the ACF to justify your conclusion
## Autocorrelation Function with seasonal and trend effects
acf(NetTot_ts)

adf.test(NetTot_ts, k = 0, alternative = "stationary")
kpss.test(NetTot_ts)

## Autocorrelation Function with trend effects (model without the seasonal component)
acf(NetTot_ts_trend, lag.max = 20)
plot(NetTot_ts_trend, main = 'Net_TOT data without Seasonal Component')

#3. If needed, difference your variables and assess each one with the ADF & KPSS
# to determine stationariy. How many, if at all, differences, did you apply?
## Autocorrelation Function with no seasonal or trend effects
NetTot_ts_diff1 = diff(NetTot_ts_trend, differences = 1)
plot(NetTot_ts_diff1)
adf.test(NetTot_ts_diff1, k = 0, alternative = "stationary")
kpss.test(NetTot_ts_diff1)
### Result: ADF: Stationary; KPSS: Stationary

#### All variables require only 1 differencing component

#4. For each variable, propose 3 alternatives ARMA models. 
## Provide estimates for each model
acf(NetTot_ts_diff1, lag.max = 20)
pacf(NetTot_ts_diff1, lag.max = 20)

## ARMA(1,1)
NetTot_arima1 = arima(NetTot_ts_diff1, order = c(1, 0, 1), method = "ML")
NetTot_arima1

## ARMA(0,1)
NetTot_arima2 = arima(NetTot_ts_diff1, order = c(0, 0, 1), method = "ML")
NetTot_arima2

## ARMA(1,0)
NetTot_arima3 = arima(NetTot_ts_diff1, order = c(1, 0, 0), method = "ML")
NetTot_arima3

coeftest(NetTot_arima1)

#5. Select the best fitting model for each of your variables 
# and include goodness of fit values
## Obtain BIC for Models
NetTot_arima1_bic = AIC(NetTot_arima1, k = log(length(NetTot_ts_diff1)))
NetTot_arima2_bic = AIC(NetTot_arima2, k = log(length(NetTot_ts_diff1)))
NetTot_arima3_bic = AIC(NetTot_arima3, k = log(length(NetTot_ts_diff1)))

NetTot_arima1_bic
NetTot_arima2_bic
NetTot_arima3_bic

## Forecast Errors
NetTot_arima1_fore = forecast(NetTot_arima1, h = 20)
NetTot_arima2_fore = forecast(NetTot_arima2, h = 20)
NetTot_arima3_fore = forecast(NetTot_arima3, h = 20)
accuracy(NetTot_arima1_fore)
accuracy(NetTot_arima2_fore)
accuracy(NetTot_arima3_fore)