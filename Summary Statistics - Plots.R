#Set the working directory
workingdirectory = "Path\\Data"
setwd(workingdirectory)
library(psych)

#1.Import the data and combine both into a single dataframe. 
##Note, both files have the column HospitalID. Use this column of data to merge the files.

hospital_data = read.table('CaliforniaHospitalData.csv', header = T, sep = ',', quote = "")
dim(hospital_data)
str(hospital_data)

personnel_data = read.table("CaliforniaHospitalData_Personnel.txt", header = T, sep = "\t", quote = "")
dim(personnel_data)
str(personnel_data)

merged_data = merge(hospital_data, personnel_data, by = 'HospitalID', all = F)
dim(merged_data)
str(merged_data)

#2.Select one of the existing hospitals in the data and create a new position for yourself. 
##Put in your first name and last name. Put today ’s date as the start date. 
###Select one of the positions as shown in the table below and fill out the data accordingly. 
####Fill in the rest of the columns as you choose. You should have one new row of data.

dim(merged_data)
new_row <- data.frame(37436, 'Fallbrook Hospital', '92028', 'www.fallbrookhospital.com', 
                      'District', 'Small/Rural', 'Charity', 501, 108960.418, 23001687.34,
                      14727466.66, 40329849, 2600695, 146, 123456, 'Dinh', 'Thu', 'F', 2, 
                      'State Board Representative', 89473, 3, '2/3/2019')

names(new_row) <- names(merged_data)
merged_data_1 = rbind(merged_data, new_row)
dim(merged_data_1)
merged_data_1[62, c('HospitalID', 'LastName', 'FirstName', 'PositionTitle', 'Compensation')]
str(merged_data_1)

#3.Using R, conduct the following tasks:
#Convert any date-time columns into a datetime datatype
str(merged_data_1$StartDate)
merged_data_1$StartDate = strptime(as.character(merged_data_1$StartDate), '%m/%d/%Y')
str(merged_data_1$StartDate)

#Remove the three primary keys from the dataframe.
dim(merged_data_1)
merged_data_2 = subset(merged_data_1, select = -c(HospitalID, Work_ID, PositionID))
dim(merged_data_2)

#Provide a summary of the mean, median, minimum value, and maximum value for each numeric variable.
##Decide which variables are numeric:
merged_data_2[, sapply(merged_data_2, is.numeric)]
##Summary of each numeric variable
summary(merged_data_2$NoFTE)
summary(merged_data_2$NetPatRev)
summary(merged_data_2$InOperExp)
summary(merged_data_2$OutOperExp)
summary(merged_data_2$OperRev)
summary(merged_data_2$OperInc)
summary(merged_data_2$AvlBeds)
summary(merged_data_2$Compensation)
summary(merged_data_2$MaxTerm)


#Export the data as a tab - delimited file and name it lastname_firstname_export,
##where lastname is your last name, and firstname is your first name.

write.table(merged_data_2, 'Dinh_Thu_Export.txt', sep = '\t', quote = FALSE)
file.exists("C:\\DATA\\MSBAN 2018\\Sem2_Spring2019\\MSIS5223\\R_Projects\\Data\\Data\\Dinh_Thu_Export.txt")


#4.After exporting your dataframe into a flat file, open the file and do the following tasks:

df = read.table('Dinh_Thu_Export.txt', header = T, sep = "\t", quote = "")

##Provide a summary of your text variables:
summary(df$Name)
summary(df$Zip)
summary(df$Website)
summary(df$LastName)
summary(df$FirstName)
summary(df$StartDate)

#Provide a summary of your categorical variables (these should not overlap with each other)
summary(df$PositionTitle)
summary(df$TypeControl)
summary(df$Teaching)
summary(df$DonorType)
summary(df$Gender)


#Create a histogram of the following variables: NoFTE, InOperExp, OutOperExp, 
##OperRev, OperInc, AvlBeds, and NetPatRev
hist(df$NoFTE, main = "NoFTE Histogram")
hist(df$InOperExp, main = "InOperExp Histogram")
hist(df$OutOperExp, main = "OutOperExp Histogram")
hist(df$OperRev, main = "OperRev Histogram")
hist(df$OperInc, main = "OperInc Histogram")
hist(df$AvlBeds, main = "AvlBeds Histogram")
hist(df$NetPatRev, main = "NetPatRev Histogram")

#Create a scatterplot of the following variables using NetPatRev as the target variable:
##NoFTE, InOperExp, OutOperExp, OperRev, OperInc, AvlBeds
###Describe the linearity and trend of each plot you created

plot(df$NoFTE, df$NetPatRev)
plot(df$InOperExp, df$NetPatRev)
plot(df$OutOperExp, df$NetPatRev)
plot(df$OperRev, df$NetPatRev)
plot(df$OperInc, df$NetPatRev)
plot(df$AvlBeds, df$NetPatRev)

#Create a boxplot and assess the lack or presence of outliers for the following variables:
##NoFTE, InOperExp, OutPerExp, OperRev, OperInc, AvlBeds, and NetPatRev
boxplot(df$NoFTE, main = "Boxplot of NoFTE")
boxplot(df$InOperExp, main = "Boxplot of InOperExp")
boxplot(df$OutOperExp, main = "Boxplot of OutOperExp")
boxplot(df$OperRev, main = "Boxplot of OperRev")
boxplot(df$OperInc, main = "Boxplot of OperInc")
boxplot(df$AvlBeds, main = "Boxplot of AvlBeds")
boxplot(df$NetPatRev, main = "Boxplot of NetPatRev")

#For the six variables in the previous task, please create a QQ plot and 
##provide your assessment of the normality
###Perform a Shapiro-Wilk test of each variable; do your results coincide with the QQ plots
qqnorm(df$NoFTE)
qqline(df$NoFTE, lty = 2)
shapiro.test(df$NoFTE) 

qqnorm(df$InOperExp)
qqline(df$InOperExp, lty = 2)
shapiro.test(df$InOperExp)

qqnorm(df$OutOperExp)
qqline(df$OutOperExp, lty = 2)
shapiro.test(df$OutOperExp)

qqnorm(df$OperRev)
qqline(df$OperRev, lty = 2)
shapiro.test(df$OperRev)

qqnorm(df$OperInc)
qqline(df$OperInc, lty = 2)
shapiro.test(df$OperInc)

qqnorm(df$AvlBeds)
qqline(df$AvlBeds, lty = 2)
shapiro.test(df$AvlBeds)

#Choose one of your variables that is not normal. Create a bar chart 
##and provide your assessment as to why normality is an issue

barplot(df$OperRev, main="Barplot of OperRev")


#Create a bar chart for four of your categorical variables: 
##Make two of them horizontal bar charts

library(ggplot2)

ggplot(df, aes(TypeControl)) +
    geom_bar(fill = "#00AFBB") +
    coord_flip()

ggplot(df, aes(PositionTitle)) +
    geom_bar(fill = "#FC4E07") +
    coord_flip()

ggplot(df, aes(DonorType)) +
    geom_bar(fill = "#0073C2FF")
 

ggplot(df, aes(Gender)) +
    geom_bar(fill = "#E7B800") 


#Based on your initial analysis, provide three observations about the data 
##that are important for your manager to know.