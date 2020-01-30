#Set the working directory
workingdirectory = "Path\\DATA"
setwd(workingdirectory)

# Read in library
library(psych) #for descriptive analysis (describe, plot, boxplot)
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


# Read in data
Enrollment_data = read.table('Enrollment_Data.csv', header = T, sep = ',', quote = "")
dim(Enrollment_data)
names(Enrollment_data)
str(Enrollment_data)

# Remove irrelevant columns:
df = subset(Enrollment_data, select = -c(CAMPUS_VISIT_2, IRSCHOOL, LEVEL_YEAR))
dim(df)
str(df)

# Convert some nominal variables from int to factor
df$Contact_Date <- as.factor(df$Contact_Date)
df$Contact_Year <- as.factor(df$Contact_Year)
df$Target_Enroll <- as.factor(df$Target_Enroll)
unique(df$Target_Enroll)

#1. Logistic Regression Model to predict Target_Enroll using all input variables  
df1 <- df[complete.cases(df),]
dim(df1)
str(df1)

Reg1 = glm(Target_Enroll ~ ., binomial, data = df1)
summary(Reg1)

#Fit Statistics
predicted <- predict(Reg1, df1, type = "response")
optCutOff1 <- optimalCutoff(df1$Target_Enroll, predicted)[1]
optCutOff1
misClassError(df1$Target_Enroll, predicted, threshold = optCutOff1)

sensitivity(df1$Target_Enroll, predicted, threshold = optCutOff1)

specificity(df1$Target_Enroll, predicted, threshold = optCutOff1)


#2. Perform PCA: 
names(df1)
df2 = df1[, c(1, 2, 7, 10, 12, 14, 15, 17, 19, 20, 21, 23, 24, 26, 27)] #numeric variables
dim(df2)
names(df2)
df3 = df1[, c(3, 4, 5, 6, 8, 9, 13, 25)] #nominal variables
dim(df3)

df2 <- df2[complete.cases(df2),]
df2.pca = princomp(df2, cor = TRUE)
print(df2.pca)
summary(df2.pca, loadings = TRUE)
# six PCs for cumulative proportion of 61.08% 

#Create a scree plot
plot(df2.pca, main = "Scree plot")

#3. Regression using retained PCA and nominal variables
PCA <- prcomp(df2, scale = TRUE)
summary(PCA)
Retained_PCA <- PCA$x[, 1:8]
dim(Retained_PCA)
library(tibble)
df4 <- data.frame(df3, Retained_PCA)
dim(df4)
str(df4)

Reg2 = glm(Target_Enroll ~ ., binomial, data = df4)
summary(Reg2)

#Fit Statistics
predicted <- predict(Reg2, df4, type = "response")
optCutOff <- optimalCutoff(df4$Target_Enroll, predicted)[1]
optCutOff
misClassError(df4$Target_Enroll, predicted, threshold = optCutOff)
sensitivity(df4$Target_Enroll, predicted, threshold = optCutOff)
specificity(df4$Target_Enroll, predicted, threshold = optCutOff)


#4.Variable Clustering
# Scale variables
df2 <- scale(df2)

# Determine the number of optimal clusters
distance <- get_dist(df2)
memory.size(max = TRUE)
set.seed(123)
fviz_nbclust(df2, kmeans, method = "wss")

# Kmeans clustering
k2 <- kmeans(df2, centers = 2, nstart = 10)
k3 <- kmeans(df2, centers = 3, nstart = 10)
k4 <- kmeans(df2, centers = 4, nstart = 10)
k5 <- kmeans(df2, centers = 5, nstart = 10)

p2 <- fviz_cluster(k2, geom = "point", df2) + ggtitle("k=2")
p3 <- fviz_cluster(k3, geom = "point", df2) + ggtitle("k=3")
p4 <- fviz_cluster(k4, geom = "point", df2) + ggtitle("k=4")
p5 <- fviz_cluster(k5, geom = "point", df2) + ggtitle("k=5")

grid.arrange(p2, p3, p4, p5, nrow = 2)
#number of optimal cluster = 3

# Summary statistics
final = kmeans(df2, centers = 3, nstart = 10)
print(final)
str(k3)
final$centers

# Cluster plot
fviz_cluster(k3, data = df2)

# Variable clustering 
var_cluster <- hclustvar(df2)
plot(var_cluster)
rect.hclust(var_cluster, 3)
rect.hclust(var_cluster, 4)


# representative variables: "distance", "TRAVEL_INIT_CNTCTS", 
#                           "SELF_INIT_CNTCTS", "satscore"


#5. Regression using representative variables and nominal variables
df5 <- df2[, c("distance", "TRAVEL_INIT_CNTCTS","SELF_INIT_CNTCTS", "satscore")]
dim(df5)
df6 <- data.frame(df3, df5)
str(df6)

Reg3 = glm(Target_Enroll ~ ., binomial, data = df6)
summary(Reg3)

#Fit Statistics
predicted <- predict(Reg3, df6, type = "response")
optCutOff <- optimalCutoff(df6$Target_Enroll, predicted)[1]
optCutOff
misClassError(df6$Target_Enroll, predicted, threshold = optCutOff)
sensitivity(df6$Target_Enroll, predicted, threshold = optCutOff)
specificity(df6$Target_Enroll, predicted, threshold = optCutOff)
