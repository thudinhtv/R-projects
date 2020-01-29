#Set up working directory
workingdirectory = "Path"
setwd(workingdirectory)

#Read in libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)
library("ClustOfVar")


#Read in data
organics = read.table('organics.csv', sep = ',', header = TRUE)
dim(organics)
str(organics)

# Convert DemAffl, DemAge, PromTime to numeric
# (because they were imported as factors)

organics$DemAffl <- as.numeric(organics$DemAffl)
organics$DemAge <- as.numeric(organics$DemAge)
organics$PromTime <- as.numeric(organics$PromTime)
str(organics)

# Subset data (using only the numeric variables for k-means)
organics = subset(organics, select = c(DemAffl, DemAge, DemCluster, PromSpend, PromTime))
names(organics)
dim(organics)

# Scale variables
organics <- na.omit(organics)
dim(organics)
organics <- scale(organics)

# Determine the number of optimal clusters
distance <- get_dist(organics)
memory.size(max = TRUE)
#set.seed(123)
#fviz_nbclust(organics, kmeans, method = "wss")

# Kmeans clustering
k2 <- kmeans(organics, centers = 2, nstart = 10)
k3 <- kmeans(organics, centers = 3, nstart = 10)
k4 <- kmeans(organics, centers = 4, nstart = 10)
k5 <- kmeans(organics, centers = 5, nstart = 10)

p2 <- fviz_cluster(k2, geom = "point", organics) + ggtitle("k=2")
p3 <- fviz_cluster(k3, geom = "point", organics) + ggtitle("k=3")
p4 <- fviz_cluster(k4, geom = "point", organics) + ggtitle("k=4")
p5 <- fviz_cluster(k5, geom = "point", organics) + ggtitle("k=5")

grid.arrange(p2, p3, p4, p5, nrow = 2)

# Summary statistics
final = kmeans(organics, centers = 2, nstart = 10)
print(final)
str(k2)
final$centers

# Cluster plot
fviz_cluster(k2, data = organics)

# Variable clustering 
tree <- hclustvar(organics)
plot(tree)
