#Set up working directory
workingdirectory = "Path"
setwd(workingdirectory)

#Read in libraries
#install.packages("arules") 
library(arules)
#install.packages("arulesViz") 
library(arulesViz)
#install.packages("tidyverse") 
library(tidyverse)
#install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse 
library(ggplot2)
#install.packages("plyr") 
library(plyr)
library(dplyr)


#Read in data
web_station = read.table('WebStation.csv', sep = ',', header = TRUE)
dim(web_station)
str(web_station)
unique(web_station$TARGET)
class(web_station)

# Missing values detect (indicate which rows have missing values)
Missing <- web_station[!complete.cases(web_station),]
Missing


# The R function paste() concatenates vectors to character and separated 
# results using collapse=[any optional charcater string ]. Here ',' is used 
webstation_trans <- ddply(web_station, c("ID"),
                         function(df1) paste(df1$TARGET,
                                            collapse = ","))
                          
webstation_trans$ID <- NULL

#Rename column to items
colnames(webstation_trans) <- c("items")
glimpse(webstation_trans)

# Write the dataframe webstation_trans to csv and read to transactions 
write.csv(webstation_trans, "WebStation_trans.csv", quote = FALSE, row.names = FALSE)
web_trans <- read.transactions('WebStation_trans.csv', format = 'basket', sep = ',', header = TRUE)
length(web_trans) # get number of observations
class(web_trans)
summary(web_trans)
inspect(web_trans[1:5])

# Create a frequency plot for the 8 services.
if (!require("RColorBrewer")) {
    # install color package of R
    install.packages("RColorBrewer")
    #include library RColorBrewer
    library(RColorBrewer)
}

itemFrequencyPlot(web_trans, topN = 8, type = "absolute",
             col = brewer.pal(8, 'Pastel2'),
             main = "Absolute Frequency Plot of Services selected by Web users")

itemFrequencyPlot(web_trans, topN = 8, type = "relative",
             col = brewer.pal(8, 'Pastel2'),
             main = "Relative Frequency Plot of Services selected by Web users")


# Generating Rules using the APRIORI algorithm (from package arules)
## Use default settings (support = 0.1, confidence = 0.8, maxlen=10)
apriori(web_trans)

## (A low support and high confidence helps to extract strong relationship 
## even for less overall co-occurrences in data)
## Use min Support=0.0001, min confidence=0.01, minlen=2, maxlen=10 
web_rules <- apriori(web_trans, parameter = list(supp = 0.0001, conf = 0.01,
                     minlen=2, maxlen = 10))
options(digits = 4)
inspect(web_rules)
quality(web_rules) # show the support, lift and confidence for all rules
summary(web_rules)

## Identify the top rules that have highest lift values
inspect(head(web_rules, by = "lift"))

highest_lift_rules <- sort(web_rules, by = "lift", decreasing = TRUE)[1:5]
inspect(highest_lift_rules)

## Identify the rules that have lowest lift values
inspect(tail(web_rules, by = "lift"))

lowest_lift_rules <- sort(web_rules, by = "lift", increasing = TRUE)[1:5]
inspect(lowest_lift_rules)

# Link graph
plot(web_rules, method = "graph", engine = "htmlwidget")

Website.association.rules <- apriori(web_trans, parameter = list(supp = 0.0001, conf = 0.01, minlen = 2, maxlen = 10), appearance = list(lhs = "WEBSITE"))
plot(Website.association.rules, method = "graph", engine = "htmlwidget")


