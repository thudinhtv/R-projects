#Import library inside R to read the SAS file given with this exercise
#install.packages("sas7bdat")
library(sas7bdat)
IRM_data = read.sas7bdat('C:\\DATA\\MSBAN 2018\\5.Spring2020\\BAN5763\\Exercises\\Exercise 3\\IRM_data.sas7bdat')
IRM_data = subset(IRM_data, select = -c(Target_I))
names(IRM_data)


#Ranking All Variables Using IV
#install.packages("Information")
library(Information)
options(scipen = 10)

## Exclude the control group
df <- subset(IRM_data, PROMO == 1)

## Ranking variables using penalized IV.
## Using ncore=2 because more than 2 is no allowed by CRAN in examples
## For real applications, leave ncore as NULL to get the default
IV <- create_infotables(data = df, y = "Target_B", parallel = FALSE)

knitr::kable(head(IV$Summary))

#plots
plot_infotables(IV, IV$Summary$Variable[1:6], show_values = TRUE, same_scales = TRUE)


# Data Partition 
library(caret)
df1 <- IRM_data[complete.cases(IRM_data),]
index <- createDataPartition(df1$Target_B, p = .7, list = FALSE)
df1_train <- df1[index,]
dim(df1_train)
df1_valid <- df1[-index,]
dim(df1_valid)


## NIV
NIV <- create_infotables(data = df1_train, valid = df1_valid, y = "Target_B", trt = "PROMO", parallel = FALSE)
knitr::kable(head(NIV$Summary))

## NWOE
knitr::kable(NIV$Tables$MMBAL)
knitr::kable(NIV$Tables$MM)
knitr::kable(NIV$Tables$ATM)
knitr::kable(NIV$Tables$LOCBAL)
knitr::kable(NIV$Tables$DEP)
knitr::kable(NIV$Tables$HMVAL)