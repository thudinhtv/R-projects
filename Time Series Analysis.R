#Set up working directory
workingdirectory = "path\\TimeSeriesData"
setwd(workingdirectory)

#Read in libraries
options(digits = 4)
library(forecast)

#Read in data
global_data = read.table('global.txt', sep = '\t', header = TRUE)
class(global_data)
summary(global_data)

#Converting dataframe to time series object
global_ts = ts(global_data, frequency = 12, start = c(1856, 1))
class(global_ts)
head(global_ts, n=24)


#1. Decompose the time series into three components and plot them
global_ts_dc = decompose(global_ts)
plot(global_ts_dc)


#2. Exponential Smoothing
global_es = HoltWinters(global_ts, beta = FALSE, gamma = FALSE, alpha = 0.3)
global_es2 = HoltWinters(global_ts, beta = FALSE, gamma = FALSE, alpha = 0.7)

layout(1:2)
plot(global_es, main = 'alpha = 0.3')
plot(global_es2, main = 'alpha = 0.7')

##Obtain estimate of alpha; do not provide a value for alpha
global_es3 = HoltWinters(global_ts, beta = FALSE, gamma = FALSE)
global_es3
plot(global_es3)

##Forecast the model beyond the known range of data
global_es3_fore = forecast(global_ts, h=8)

##Forecasts with 80% and 95% intervals
global_es3_fore

##Look at forecasted values
plot(global_es3_fore)

##Assess constant variance
plot(global_es3_fore$residuals)
lines(c(2010, -0.138), c(0, -0.138), col = 'red')

##Define a function to forecast error
plotForecastErrors = function(forecasterrors, forecasttitle) {
    forecasterrors = na.omit(forecasterrors)
    mybinsize = IQR(forecasterrors) / 4
    mysd = sd(forecasterrors)
    mymin = min(forecasterrors) - mysd * 5
    mymax = max(forecasterrors) + mysd * 3
    mynorm <- rnorm(10000, mean = 0, sd = mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col = "red", freq = FALSE, breaks = mybins, main = forecasttitle)
    myhist <- hist(mynorm, plot = FALSE, breaks = mybins)
    points(myhist$mids, myhist$density, type = "l", col = "blue", lwd = 2)
}

##Assess normality of residuals
plotForecastErrors(global_es3_fore$residuals, 'Assessing Normal Distribution')


#3. Trend Exponential Smoothing

global_es4 = HoltWinters(global_ts,
                            alpha = 0.2,
                            beta = 0.4,
                            gamma = FALSE,
                            l.start = -0.153,
                            b.start = 0.05)

global_es5 = HoltWinters(global_ts,
                            alpha = 0.2,
                            beta = 0.8,
                            gamma = FALSE,
                            l.start = -0.153,
                            b.start = 0.05)

global_es6 = HoltWinters(global_ts,
                            alpha = 0.7,
                            beta = 0.4,
                            gamma = FALSE,
                            l.start = -0.153,
                            b.start = 0.05)

global_es7 = HoltWinters(global_ts,
                            alpha = 0.7,
                            beta = 0.8,
                            gamma = FALSE,
                            l.start = -0.153,
                            b.start = 0.05)

par(mfrow = c(2, 2))
plot(global_es4, main = 'a=0.2, b=0.4')
plot(global_es5, main = 'a=0.2, b=0.8')
plot(global_es6, main = 'a=0.7, b=0.4')
plot(global_es7, main = 'a=0.7, b=0.8')
par(mfrow = c(1, 1))

##Allow the model to determine alpha and beta
global_es8 = HoltWinters(global_ts,
                            gamma = FALSE,
                            l.start = -0.153,
                            b.start = 0.05)

global_es8

##Create a new time series object (the first 5 years of the data)
global_ts2 = ts(global_data, start = 1, end = 60)

global_es9 = HoltWinters(global_ts2,
                            gamma = FALSE,
                            l.start = -0.153,
                            b.start = 0.05)

global_es9
plot(global_es9)

global_es9_fore = forecast(global_es9, h = 8)
plot(global_es9_fore)

#Assess constant variance
plot(global_es9_fore$residuals)
lines(c(120, -0.138), c(0, -0.138), col = 'red')

##Assess normal distribution
plotForecastErrors(global_es9_fore$residuals, 'Assessing Normal Distribution')


# 4.Holt-Winters Exponential Smoothing
#Remove season to use Trend-Adjusted Exponential Smoothing
global_ts_trend = global_ts - global_ts_dc$seasonal

global_es = HoltWinters(global_ts_trend,
                        gamma = FALSE)

global_es
plot(global_es)

#Leave season in the model
global_es2 = HoltWinters(global_ts,
                        gamma = FALSE)

#Forecast the next 8 periods for both
global_es_fore = forecast(global_es, h = 8)
global_es_fore2 = forecast(global_es2, h = 8)

#Assess constant variance
par(mfrow = c(2, 1))
plot(global_es_fore$residuals, main = 'Global Temperature: No Seasonal Component')
lines(c(2010,-0.138), c(0, -0.138), col = 'red')

plot(global_es_fore2$residuals, main = 'Global Temperature: With Seasonal Component')
lines(c(2010, -0.138), c(0, -0.138), col = 'red')

#Assess normal distribution
plotForecastErrors(global_es_fore$residuals, 'Global Temperature: No Seasonal Component')

plotForecastErrors(global_es_fore2$residuals, 'Global Temperature: With Seasonal Component')


#5. Create a boxplot to look at seasonality
#Descriptive Analysis
start(global_ts)
end(global_ts)
frequency(global_ts)
plot(global_ts, ylab = "Global Temperature")

#boxplot
plot(aggregate(global_ts))
boxplot(global_ts ~ cycle(global_ts))


#6.Compare standard deviations to see if seasonal effect does exist
sd(global_ts)
sd(global_ts - global_ts_dc$seasonal)
