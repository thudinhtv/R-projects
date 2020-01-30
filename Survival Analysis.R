#Import the data into R
workingdirectory = "Path"
setwd(workingdirectory)

Mortgage_data = read.table('Mortgage.csv', header = T, sep = ',', quote = "")
dim(Mortgage_data)
str(Mortgage_data)
head(Mortgage_data)

# Format the date variables
Mortgage_data$start_date = as.Date(Mortgage_data$start_date, format = "%d%b%Y")
str(Mortgage_data$start_date)

Mortgage_data$end_date = as.Date(Mortgage_data$end_date, format = "%d%b%Y")
str(Mortgage_data$end_date)

# Explore target variable:
unique(Mortgage_data$event)
table(Mortgage_data$event) 
library(ggplot2)
ggplot(Mortgage_data, aes(event)) +
    geom_bar(fill = "#0073C2FF")


# install.packages("survival")
# install.packages("survminer")
# Loading the packages
library("survival")
library("survminer")

# Create the response/target variable:
S <- Surv(
  time = as.numeric(Mortgage_data$start_date),
  time2 = as.numeric(Mortgage_data$end_date),
  event = Mortgage_data$event)

#  Building the Cox model
model <- coxph(S ~ vintage + cred_score + DBT_RATIO,
               data = Mortgage_data)

summary(model)

# Hazard ratio
ggforest(model, data = Mortgage_data)

#Create a survival curve from the cox model
Cox_curve <- survfit(model)
plot(Cox_curve)

