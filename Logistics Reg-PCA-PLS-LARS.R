#Set the working directory
workingdirectory = "Path\\DATA"
setwd(workingdirectory)

# Read in library
library(psych) #for descriptive analysis (describe, plot, boxplot)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("InformationValue")
library(InformationValue)
library(cluster)
library(factoextra)
library(FactoMineR)
library(gridExtra)
library("ClustOfVar")
library(tibble) #joining data
#install.packages("caret", dependencies = c("Depends", "Suggests"))
library(caret) # data partition
library(car)
library(MASS)
library(dplyr)


# Read in data
Enrollment_data = read.table('Enrollment_Data.csv', header = T, sep = ',', quote = "")
dim(Enrollment_data)
names(Enrollment_data)
str(Enrollment_data)
sapply(Enrollment_data, function(x) sum(is.na(x)))
sapply(Enrollment_data, function(x) length(unique(x)))

# Remove irrelevant columns:
df = subset(Enrollment_data, select = -c(CAMPUS_VISIT_2, IRSCHOOL, LEVEL_YEAR))
dim(df)
str(df)

# Convert some nominal variables from int to factor
df$Contact_Date <- as.factor(df$Contact_Date)
df$Contact_Year <- as.factor(df$Contact_Year)
df$Target_Enroll <- as.factor(df$Target_Enroll)
unique(df$Target_Enroll)

df1 <- df[complete.cases(df),]
sapply(df1, function(x) sum(is.na(x)))
dim(df1)
str(df1)

#1. Logistic Regression Model to predict Target_Enroll using stepwise selection  
# Data Partition 
index <- createDataPartition(paste(df1$CONTACT_CODE1, df1$Contact_Year,
                             df1$Target_Enroll), p = .7, list = FALSE)
df1_train <- df1[index,]
dim(df1_train)
df1_test <- df1[-index,]
dim(df1_test)

# Regression model with stepwise selection
Reg1 <- glm(Target_Enroll ~ ., data = df1_train, family = binomial) %>%
    stepAIC(trace = FALSE)
summary(Reg1)

#Fit Statistics
fitted_results1 <- predict(Reg1, df1_test, type = "response")
optCutOff1 <- optimalCutoff(df1_test$Target_Enroll, fitted_results1)[1]
optCutOff1
fitted_results1 <- ifelse(fitted_results1 > optCutOff1, 1, 0)
misClasificError1 <- mean(fitted_results1 != df1_test$Target_Enroll)
print(paste('Accuracy', 1 - misClasificError1))


#2. PCA for variable selection + regression model using retained PCAs + nominal variables
 
# Subset new data set that includes only numeric variables
df2 = df1[, c(1, 2, 7, 10, 12, 14, 15, 17, 19, 20, 21, 23, 24, 26, 27)] 
dim(df2)
names(df2)

# Subset new data set that includes only nominal variables (including target var as well)
df3 = df1[, c(3, 4, 5, 6, 8, 9, 25)] 
dim(df3)
names(df3)

# PCA
df2.pca = princomp(df2, cor = TRUE)
df2.pca$sdev ^ 2
summary(df2.pca, loadings = TRUE)
# six PCs for cumulative proportion of 61.08% 

#Create a scree plot
plot(df2.pca, main = "Scree plot")

# Regression using retained PCAs
PCA <- prcomp(df2, scale = TRUE)
PCA$sdev ^ 2
summary(PCA)
Retained_PCA <- PCA$x[, 1:6]
dim(Retained_PCA)
df4 <- data.frame(df3, Retained_PCA)
dim(df4)
names(df4)

#data partition for df4
index_df4 <- createDataPartition(paste(df4$CONTACT_CODE1, df4$Contact_Year, df4$Target_Enroll),
                                 p = .7, list = FALSE)
df4_train <- df4[index_df4,]
dim(df4_train)
df4_test <- df4[-index_df4,]
dim(df4_test)
str(df4_test)

Reg2 = glm(Target_Enroll ~ ., binomial, data = df4_train)
summary(Reg2)

#Fit Statistics
fitted_results2 <- predict(Reg2, df4_test, type = "response")
optCutOff2 <- optimalCutoff(df4_test$Target_Enroll, fitted_results2)[1]
optCutOff2
fitted_results2 <- ifelse(fitted_results2 > optCutOff2, 1, 0)
misClasificError_2 <- mean(fitted_results2 != df4_test$Target_Enroll)
print(paste('Accuracy', 1 - misClasificError_2))


#3.Variable Clustering for variable selection + regression model using representative variables from Var clustering + nominal variables

# Variable clustering 
df1_train_numeric <- df1_train[, c(1, 2, 7, 10, 12, 14, 15, 17, 19, 20, 21, 23, 24, 26, 27)]
names(df1_train_numeric)
var_cluster <- hclustvar(df1_train_numeric)
plot(var_cluster)
rect.hclust(var_cluster, 5)

# representative variables: "hscrat", "TRAVEL_INIT_CNTCTS", "SELF_INIT_CNTCTS", "init_span", "satscrore"

# Regression using representative variables and nominal variables
names(df1_train)
df5_train <- df1_train[, c(3, 4, 5, 6, 8, 9, 10, 12, 20, 21, 25, 27)]
df5_test <- df1_test[, c(3, 4, 5, 6, 8, 9, 10, 12, 20, 21, 25, 27)]
names(df5_train)
names(df5_test)

Reg3 = glm(Target_Enroll ~ ., binomial, data = df5_train)
summary(Reg3)

#Fit Statistics
fitted_results3 <- predict(Reg3, df5_test, type = "response")
optCutOff3 <- optimalCutoff(df5_test$Target_Enroll, fitted_results3)[1]
optCutOff3
fitted_results3 <- ifelse(fitted_results3 > optCutOff3, 1, 0)
misClasificError_3 <- mean(fitted_results3 != df5_test$Target_Enroll)
print(paste('Accuracy', 1 - misClasificError_3))


#4. PLS for variable selection + regression model using PLS selected variables + nominal variables
library(pls)

pls_train = df1_train[, c(1, 2, 7, 8, 10, 12, 14, 15, 17, 19, 20, 21, 23, 24, 26, 27)]
str(pls_train)
pls_train$Target_Enroll <- as.numeric(pls_train$Target_Enroll)
pls_model_1 = plsr(Target_Enroll ~ ., data = pls_train, validation = "CV")

# Find the number of dimensions with lowest cross validation error
cv = RMSEP(pls_model_1)
print(cv)
best_dims = which.min(cv$val[estimate = "adjCV",,]) - 1

# Rerun the model
pls_model_2 = plsr(Target_Enroll ~ ., data = pls_train, ncomp = best_dims)
summary(pls_model_2)


coefficients = coef(pls_model_2)
coefficients = sort(coefficients)
coefficients

# vars selected: hscrat, int2rat, int1rat, CAMPUS_VISIT, REFERRAL_CNTCTS, TOTAL_CONTACTS,
#  mailq, SELF_INIT_CNTCTS, TRAVEL_INIT_CNTCTS, init_span, SOLICITED_CNTCTS, telecq

# Regression using selected variables from PLS and nominal variables
names(df1_train)
df6_train <- df1_train[, -c(1, 7, 11, 13, 18, 20, 22)]
df6_test <- df1_test[, - c(1, 7, 11, 13, 18, 20, 22)]
names(df6_train)
dim(df6_train)
names(df6_test)

Reg4 = glm(Target_Enroll ~ ., binomial, data = df6_train)
summary(Reg4)

#Fit Statistics
fitted_results4 <- predict(Reg4, df6_test, type = "response")
optCutOff4 <- optimalCutoff(df6_test$Target_Enroll, fitted_results4)[1]
optCutOff4
fitted_results4 <- ifelse(fitted_results4 > optCutOff4, 1, 0)
misClasificError_4 <- mean(fitted_results4 != df6_test$Target_Enroll)
print(paste('Accuracy', 1 - misClasificError_4))


#5. LARS for variable selection + regression model using LARS selected variables + nominal variables
#Method 1: 
library(lars)
names(pls_train)
x <- model.matrix(Target_Enroll ~ ., data = pls_train)
x = x[, -4]
y = pls_train$Target_Enroll
LARS_model = lars(x, y, type = "lar")
summary(LARS_model)
coef(LARS_model)

#Method 2:
cv <- lars::cv.lars(x, y, plot.it = FALSE)
ideal_l1_ratio <- cv$index[which.max(cv$cv - cv$cv.error <= min(cv$cv))]
obj <- lars::lars(x, y)
scaled_coefs <- scale(obj$beta, FALSE, 1 / obj$normx)
l1 <- apply(X = scaled_coefs, MARGIN = 1, FUN = function(x) sum(abs(x)))
coef(obj)[which.max(l1 / tail(l1, 1) > ideal_l1_ratio),]

# Method 3: 
library(glmnet)
"%ni%" <- Negate("%in%")
glmnet1<-cv.glmnet(x, y, type.measure='mse',nfolds=5,alpha=.5)
c<-coef(glmnet1,s='lambda.min',exact=TRUE)
inds<-which(c!=0)
variables<-row.names(c)[inds]
variables<-variables[variables %ni% '(Intercept)']
print(variables)

# Variables selected from LARS: "avg_income", "CAMPUS_VISIT", "hscrat",
# "init_span", "int1rat", "int2rat", "mailq", "REFERRAL_CNTCTS", 
# "satscore", "SELF_INIT_CNTCTS", "telecq", "TOTAL_CONTACTS", "TRAVEL_INIT_CNTCTS"

# Regression using selected variables from LARS and nominal variables
names(df1_train)
df7_train <- df1_train[, - c(7, 11, 13, 18, 22, 23)]
df7_test <- df1_test[, - c(7, 11, 13, 18, 22, 23)]
names(df7_train)
dim(df7_train)
names(df7_test)

Reg5 = glm(Target_Enroll ~ ., binomial, data = df7_train)
summary(Reg5)

#Fit Statistics
fitted_results5 <- predict(Reg5, df7_test, type = "response")
optCutOff5 <- optimalCutoff(df7_test$Target_Enroll, fitted_results5)[1]
optCutOff5
fitted_results5 <- ifelse(fitted_results4 > optCutOff5, 1, 0)
misClasificError_5 <- mean(fitted_results5 != df7_test$Target_Enroll)
print(paste('Accuracy', 1 - misClasificError_5))

str(df7_test)
sensitivity(df7_test$Target_Enroll, fitted_results5, threshold = optCutOff5)
specificity(df7_test$Target_Enroll, fitted_results5, threshold = optCutOff5)




# Data Partition - method 2
set.seed(12345)
n_all = nrow(df1) #total number of rows in data
ntrain = floor(0.7 * n_all) # number of rows for train,70%
ntest = floor(0.3 * n_all) # number of rows for test, 30%
index = seq(1:n_all)
trainindex = sample(index, ntrain) #train data set
testindex = index[-trainindex] #test data set

train = df1[trainindex,]
dim(train)
test = df1[testindex,]
dim(test)
str(test)