#Set the working directory
workingdirectory = "Path\\Data"
setwd(workingdirectory)

install.packages("tree")
library(tree)

#1. PCA and Factor Analysis
#Read in data
ect_data = read.table('ect_data.txt', header = T, sep = "\t")
dim(ect_data)
str(ect_data)

#Perform PCA
ect_data.pca = princomp(ect_data, cor = FALSE)
ect_data.pca$sdev ^ 2

#Create a scree plot
plot(ect_data.pca, main = "Scree plot of Attitude, Intent, Peruse, Satisfaction")

#Factor analysis
ect_data.FA = factanal(~attitude1_01 + attitude1_02 + attitude1_03 + attitude1_04 + intent1_01
                       + intent1_02 + intent1_03 + intent1_04 + peruse01 + peruse02 + peruse03
                       + peruse04 + satis01 + satis02 + satis03 + satis04, factors = 4,
                       rotation = "varimax", scores = "none", data = ect_data)

ect_data.FA

#Remove attitude1_01, attitude1_02, and intent1_04 (values < 0.70) and
##run the analysis one more time
ect_data.FA = factanal(~attitude1_03 + attitude1_04 + intent1_01 + intent1_02 + intent1_03
                       + peruse01 + peruse02 + peruse03 + peruse04 + satis01 + satis02
                       + satis03 + satis04, factors = 4, rotation = "varimax", scores = "none", data = ect_data)

ect_data.FA

#2.Cluster Analysis with R
##Read in data
car_data = read.table("car.test.frame.txt", header = T, sep = "\t")
str(car_data)
unique(car_data$Type)

##Allow the R Graphics Windown to display 4 graphics
par(mfrow = c(2, 2))

##Look at the data without using a cluster algorithm, no color coded
plot(car_data$Weight, car_data$Price, pch = 18, main="Type Variable")

##Add color based on the "grouping" variable
plot(car_data$Weight, car_data$Price, pch = 18, col = car_data$Type, main = "Type Variable")

##Perform the K-Means analysis with 4 variables Price, Weight, Disp., HP
### Restrict the data to 6 distinct groups
km = kmeans(data.frame(car_data$Price, car_data$Weight, car_data$Disp., car_data$HP), 6)
plot(car_data$Weight, car_data$Price, col = km[[1]], main = "6 K-Means Groups")

### Restrict the data based on using 4 groups instead of 6
km2 = kmeans(data.frame(car_data$Weight, car_data$Price, car_data$Disp., car_data$HP), 4)
plot(car_data$Weight, car_data$Price, col = km2[[1]], main = "4 K-Means Groups")

### Reset the number of figures to display to just 1
par(mfrow = c(1, 1))

### Assess the misclassification for 6 groups
table(km[[1]], car_data$Type)

##Hierarchical Clustering Analysis - Agglomerative
car_data2 = subset(car_data, select = -c(Country, Type))
names(car_data2)
plot(hclust(dist(car_data2)), main = "Agglomerative Clustering")

