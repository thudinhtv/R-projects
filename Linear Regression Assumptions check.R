#Set the working directory
workingdirectory = "Path\\Data"
setwd(workingdirectory)

# Read in library
library(psych)      #for descriptive analysis (describe, plot, boxplot)
library(ggplot2)
#library(Hmisc)     #For rcorr() -> Only load this after using psych; it overrides psych
library(car)        #for vif() and Durbin-Watson test
library(fmsb)       #for VIF function

# Read in data
hospital_data = read.table('CaliforniaHospitalData.csv', header = T, sep = ',', quote = "")
dim(hospital_data)
names(hospital_data)
str(hospital_data)

# Remove irrelevant columns: HospitalID, Name, Zip, Website
df = subset(hospital_data, select = -c(HospitalID, Name, Zip, Website))
dim(df)
names(df)
str(df)

#1. Logistic Regression Model using DonorType as the target variable 
# and all other numerical variables as independent variables 

# Subset DonorType and all the numerical columns to a new dataframe 
df1 = df[, c(3, 4, 5, 6, 7, 8, 9, 10)]
names(df1)
str(df1)
unique(df1$DonorType)

# Logistic Regression Model using all numerical columns as indenpendent variables
df1_LogReg = glm(DonorType ~ ., binomial, data = df1)
summary(df1_LogReg)

# Correlation analysis:
df3 = df[, c(4, 5, 6, 7, 8, 9, 10)]
df3_corr = cor(df3)
df3_corr

# Logistic Regression Model using OperInc as independent variable
df1_LogReg = glm(DonorType ~ OperInc, binomial, data = df1)
summary(df1_LogReg)
anova(df1_LogReg, test = "Chisq")

# Making predictions to evaluate the model
install.packages("tidyverse")
library(tidyverse)

probabilities <- df1_LogReg %>% predict(df1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "Charity", "Alumni")
mean(predicted.classes == df1$DonorType)


#2. Multiple Regression Model using OperInC as the target variable
# and all numerical data as independent variables

# Subset all the numerical columns to a new dataframe and explore data
df2 = df[, c(4, 5, 6, 7, 8, 9, 10)]
str(df2)
summary(df2)
describe(df2$OperInc)
boxplot(df2$OperInc)

# Multiple Regression
df2_reg = lm(df2$OperInc~df2$NoFTE+df2$NetPatRev+df2$InOperExp+df2$OutOperExp+df2$OperRev+df2$AvlBeds)
summary(df2_reg)

# Test Assumption 1: Linearity <-> create scatter plot
pairs(df2, panel=panel.smooth)

# Test Assumption 2: Multicollinearity <-> Correlation analysis/ VIF score
## Correlation
df2_corr = cor(df2)
df2_corr
rcorr(as.matrix(df2))

## Calculate vif using library car
vif(df2_reg)

# Test Assumption 3: Independence
durbinWatsonTest(df2_reg)

# Test Assumption 4 - Homoscedasticity and Assumption 5 - Normality
plot(df2_reg)           #hit 'Enter' to move through the 4 plots


#3. Multiple Regression Model using OperInC as the target variable
# and InOperExp, OutOperExp, OperRev as 3 independent variables
df4 = df[, c(6, 7, 8, 9)]
str(df4)

df4_reg = lm(df4$OperInc ~ df4$InOperExp + df4$OutOperExp + df4$OperRev)
summary(df4_reg)

# Test Assumption 1: Linearity <-> create scatter plot
pairs(df4, panel = panel.smooth)

# Test Assumption 2: Multicollinearity <-> Correlation analysis/ VIF score
## Correlation
rcorr(as.matrix(df4))
## Calculate vif using library car
vif(df4_reg)

# Test Assumption 3: Independence
durbinWatsonTest(df4_reg)

# Test Assumption 4 - Homoscedasticity and Assumption 5 - Normality
plot(df4_reg) #hit 'Enter' to move through the 4 plots
