#Set up working directory
workingdirectory = "path\\TimeSeriesData"
setwd(workingdirectory)

#Read in libraries
options(digits = 4)
library(forecast)

#Read in data
HospitalFinancial_data = read.table('CaliforniaHospital_FinancialData.txt', sep = '\t', header = TRUE)
str(HospitalFinancial_data)
head(HospitalFinancial_data)

#Subset the NonOpRev column to new dataset
NonOpRev_data = subset(HospitalFinancial_data, select = c("NONOP_REV"))
names(NonOpRev_data)
summary(NonOpRev_data)
class(NonOpRev_data)

#Converting dataframe to time series object
NonOpRev_ts = ts(NonOpRev_data, frequency = 4, start = c(2007, 1))
class(NonOpRev_ts)
NonOpRev_ts


#1. Create a boxplot to look at seasonality
#Descriptive Analysis
start(NonOpRev_ts)
end(NonOpRev_ts)
frequency(NonOpRev_ts)
plot(NonOpRev_ts, ylab = "Non-Operating Revenue")

#boxplot
plot(aggregate(NonOpRev_ts))
boxplot(NonOpRev_ts ~ cycle(NonOpRev_ts))

#2. Creating a plot of the components
NonOpRev_ts_dc = decompose(NonOpRev_ts)
plot(NonOpRev_ts_dc)

#Compare standard deviations to see if seasonal effect does exist
sd(NonOpRev_ts)
sd(NonOpRev_ts - NonOpRev_ts_dc$seasonal)


#3.Use a predetermined smoothing parameter
NonOpRev_es1 = HoltWinters(NonOpRev_ts, alpha = 0.3)
NonOpRev_es1
NonOpRev_es2 = HoltWinters(NonOpRev_ts, alpha = 0.7)
NonOpRev_es2

layout(1:2)
plot(NonOpRev_es1, main = 'HoltWinters with alpha = 0.3')
plot(NonOpRev_es2, main = 'HoltWinters with alpha = 0.7')


#4.Holt-Winters Exponential Smoothing: allow R to determine the smoothing parameters
NonOpRev_es3 = HoltWinters(NonOpRev_ts)
NonOpRev_es3

plot(NonOpRev_es2, main = 'HoltWinters with alpha = 0.7')
plot(NonOpRev_es3, main = 'HoltWinters with R-decided alpha')

#5. Assess model 2 and 3 for homoscedasticity and normality
##Forecast the next 8 periods
NonOpRev_es2_fore = forecast(NonOpRev_es2, h = 8)
NonOpRev_es2_fore

NonOpRev_es3_fore = forecast(NonOpRev_es3, h = 8)
NonOpRev_es3_fore

##Assess constant variance
par(mfrow = c(2, 1))
plot(NonOpRev_es2_fore$residuals, main = 'Non-Operating Revenue with alpha=0.7')
lines(c(2016, 0), c(2008, 0), col = 'Red')

plot(NonOpRev_es3_fore$residuals, main = 'Non-Operating Revenue with alpha=0.9467')
lines(c(2016, 0), c(2008, 0), col = 'Red')

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

##Assess normal distribution
plotForecastErrors(NonOpRev_es2_fore$residuals, 'Non-Operating Revenue with alpha=0.7')
plotForecastErrors(NonOpRev_es3_fore$residuals, 'Non-Operating Revenue with alpha=0.9467')


#6. Forecast 40 periods into future for both models. Assess both for homoscedasticity 
# and normality. Which performs better?
##Forecast the next 40 periods
NonOpRev_es4_fore = forecast(NonOpRev_es2, h = 40)
NonOpRev_es5_fore = forecast(NonOpRev_es3, h = 40)

##Assess constant variance
plot(NonOpRev_es4_fore$residuals, main = 'Non-Operating Revenue with alpha=0.7')
lines(c(2016, 0), c(2008, 0), col = 'Red')
plot(NonOpRev_es5_fore$residuals, main = 'Non-Operating Revenue with alpha=0.9467')
lines(c(2016, 0), c(2008, 0), col = 'Red')

##Assess normal distribution
plotForecastErrors(NonOpRev_es4_fore$residuals, 'Non-Operating Revenue with alpha=0.7')
plotForecastErrors(NonOpRev_es5_fore$residuals, 'Non-Operating Revenue with alpha=0.9467')





