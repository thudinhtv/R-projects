#Set the working directory in R
workingdirectory = "Path\\Data"
setwd(workingdirectory)

# Read in library
library(psych) #for descriptive analysis 
library(ggplot2)
library(plyr)  #for frequency count
library(car) #for Durbin-Watson test, vif score, binning
library(dummies)  #for dummy variables

#Read in data and descriptive analysis
crop_data = read.table('splityield.txt', sep = '\t', header = TRUE)

dim(crop_data)
names(crop_data)
str(crop_data)
summary(crop_data)

describe(crop_data$yield)

count(crop_data, 'block')
count(crop_data, 'irrigation')
count(crop_data, 'density')
count(crop_data, 'fertilizer')

#Check for missing values
is.na(crop_data)
complete.cases(crop_data)               # return a logical vector indicating which cases are complete
crop_data[!complete.cases(crop_data),]     #list of rows that have missing values

# Create dummy variables for the column 'block'
unique(crop_data$block)

block_dummy = dummy(crop_data$block, sep = '_')
colnames(block_dummy)

##Convert the dummy variables into a dataframe
block_dummy = as.data.frame(block_dummy)

##Add back into the dataframe
crop_data = data.frame(crop_data, block_dummy)
str(crop_data)
unique(crop_data$block_A)
unique(crop_data$block_B)
unique(crop_data$block_C)
unique(crop_data$block_D)


# Create dummy variables for the column 'irrigation'
unique(crop_data$irrigation)

irrigation_dummy = dummy(crop_data$irrigation, sep = '_')
colnames(irrigation_dummy)

##Convert the dummy variables into a dataframe
irrigation_dummy = as.data.frame(irrigation_dummy)

##Add back into the dataframe
crop_data = data.frame(crop_data, irrigation_dummy)
str(crop_data)


# Create dummy variables for the column 'density'
unique(crop_data$density)

density_dummy = dummy(crop_data$density, sep = '_')
colnames(density_dummy)

##Convert the dummy variables into a dataframe
density_dummy = as.data.frame(density_dummy)

##Add back into the dataframe
crop_data = data.frame(crop_data, density_dummy)
str(crop_data)


# Create dummy variables for the column 'fertilizer'
unique(crop_data$fertilizer)

fertilizer_dummy = dummy(crop_data$fertilizer, sep = '_')
colnames(fertilizer_dummy)

#Change the column names
colnames(fertilizer_dummy) = c('fer_Nitrogen', 'fer_Potassium', 'fer_Nitrogen_Potasium')

##Convert the dummy variables into a dataframe
fertilizer_dummy = as.data.frame(fertilizer_dummy)

##Add back into the dataframe
crop_data = data.frame(crop_data, fertilizer_dummy)
colnames(crop_data)
str(crop_data)
head(crop_data)


#Regression Analysis using all dummies variables 
#(after leaving the last indicator variable out of each set of dummy variables)
crop_reg1 = lm(crop_data$yield ~ crop_data$block_A + crop_data$block_B
              + crop_data$block_C + crop_data$irrigation_control +
              crop_data$density_high + crop_data$density_low +
              crop_data$fer_Nitrogen + crop_data$fer_Nitrogen_Potasium)

summary(crop_reg1)
plot(crop_reg1)

# Regression Analysis using Irrigation dummies, Density dummies, and Fertilizer dummies

crop_reg2 = lm(crop_data$yield ~ crop_data$irrigation_control +
              crop_data$density_high + crop_data$density_low +
              crop_data$fer_Nitrogen + crop_data$fer_Nitrogen_Potasium)

summary(crop_reg2)
plot(crop_reg2)
