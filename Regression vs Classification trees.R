#Set the working directory
workingdirectory = "Path\\Data"
setwd(workingdirectory)

library(tree)

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

#1.Regression trees using OperInC as the target variable
# and all numerical columns as predictor variables

# Subset all the numerical columns to a new dataframe and 
# set OperInc to be the first column of the dataframe
df1 = df[, c(9, 4, 5, 6, 7, 8, 10)]
names(df1)
str(df1)

## Create a tree
OperInc_tree = tree(df1)
OperInc_tree

## Visual representation of the tree
plot(OperInc_tree) #plot the tree structure
text(OperInc_tree) #attach text to tree plot

## Explain splitting using Deviance: NetPatRev plotted against OperInc
low = (df1$NetPatRev < 2.24938e+006) #use the first split value
tapply(df1$OperInc, low, mean)
plot(df1$NetPatRev, df1$OperInc, pch = 16)
abline(v = 2.24938e+006, lty = 2)
lines(c(0, 2.24938e+006), c(-11390000, -11390000)) #Low-end mean
lines(c(2.24938e+006, max(df1$NetPatRev)), c(102800000, 102800000)) #High-end mean


#2.Regression tree using OperRev as the target variable

# Subset all the numerical columns to a new dataframe and 
# set OperRev to be the first column of the dataframe
df2 = df[, c(8, 4, 5, 6, 7, 9, 10)]
names(df2)

## Create a tree
OperRev_tree = tree(df2)
OperRev_tree

## Visual representation of the tree
plot(OperRev_tree) #plot the tree structure
text(OperRev_tree) #attach text to tree plot

## Explain splitting using Deviance: NetPatRev plotted against OperRev
low = (df2$NetPatRev < 1.8902e+006)
tapply(df2$OperRev, low, mean)
plot(df2$NetPatRev, df2$OperRev, pch = 16)
abline(v = 1.8902e+006, lty = 2)
lines(c(0, 1.8902e+006), c(1.233e+08, 1.233e+08)) #Low-end mean
lines(c(1.8902e+006, max(df2$NetPatRev)), c(1.282e+09, 1.282e+09)) #High-end mean


#3. Classification trees using TypeControl as the target variable 
# and all other variables as independent variables

# Set TypeControl to be the first column 
df3=df
names(df3)
unique(df3$TypeControl)

# Create a tree
TypeControl_tree = tree(TypeControl ~ ., df3)
TypeControl_tree

# Plot the tree
plot(TypeControl_tree)
text(TypeControl_tree)

## Prune the tree to simplify it.
TypeControl_prune_tree = prune.tree(TypeControl_tree)
TypeControl_prune_tree
plot(TypeControl_prune_tree)

#### using 5 terminal nodes
TypeControl_prune_tree2 = prune.tree(TypeControl_tree, best = 5)
TypeControl_prune_tree2

plot(TypeControl_prune_tree2)
text(TypeControl_prune_tree2)

#4. Classification trees using DonorType as the target variable 
# and all other variables as independent variables 

# set DonorType to be the first column of the dataframe
df4 = df[, c(3, 1, 2, 4, 5, 6, 7, 8, 9, 10)]
names(df4)
unique(df4$DonorType)

# Create a tree
DonorType_tree = tree(DonorType ~ ., df4)
DonorType_tree

# Plot the tree
plot(DonorType_tree)
text(DonorType_tree)





