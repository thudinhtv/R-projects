#Set the working directory
workingdirectory = "Path\\Data"
setwd(workingdirectory)

# Open the file car.test.frame.txt within R using read.table()
car_data = read.table("car.test.frame.txt", header = T, sep = "\t")

# Find out what the column header names are
names(car_data)

# Determine the number of columns
ncol(car_data)

# Determine the number of rows
nrow(car_data)

#Test your dataframe to determine which columns are categorical
car_data[, sapply(car_data, is.factor)]
str(car_data)

# How many unique values does Type have ?
unique(car_data$Type)

# How many unique values does Country have ?
unique(car_data$Country)

# What is the value of row 57, column 3 ?
car_data[57, 3]

# What are the values for row 24 ?
car_data[24,]

# Using three different methods, select row 29 with columns 1, 2, 3
car_data[29, 1:3]
car_data[29, c(1, 2, 3)]
car_data[29, c("Price", "Country", "Reliability")]

# Using two different ways, select row 45 with columns 3 and 7
car_data[45, c(3, 7)]
car_data[45, c("Reliability", "Disp.")]

# Create a new dataframe for the column HP using two different methods
car_data[, "HP"]
car_data[, 8]


# Select compact cars that have a reliability greater than and equal to 4.
car_data[car_data$Type == "Compact" & car_data$Reliability >= 4,]

# Find compact cars that have a reliability greater than and equal to 3 from Japan, but not from the US
car_data[car_data$Type == "Compact" & car_data$Reliability >= 3 & car_data$Country == "Japan",]

# How many cars are manufactured in the USA / Japan ?
nrow(car_data[car_data$Country == "USA/Japan",])

# How many cars are manufactured in the US or Japan ?
nrow(car_data[car_data$Country == "USA" | car_data$Country == "Japan",])

# How many cars are manufactured in the US or Japan with a reliability greater than and equal to 4 ?
nrow(car_data[(car_data$Country == "USA" | car_data$Country == "Japan") & car_data$Reliability >= 4,])

# Create a subsample of 70 % of your data
subsample.num = round(nrow(car_data) * 0.7, 0)
subsample.num
car_data.subsample = car_data[sample(1:nrow(car_data), subsample.num, replace = F),]
car_data.subsample

# Create samples for a 8 - fold cross validation test; save each subsample as a new dataframe
samp.size = nrow(car_data) / 8

indices.one = sort(sample(seq_len(nrow(car_data)), size = samp.size))
indices.not_1 = setdiff(seq_len(nrow(car_data)), indices.one)
indices.two = sort(sample(indices.not_1, size = samp.size))
indices.not_12 = setdiff(indices.not_1, indices.two)
indices.three = sort(sample(indices.not_12, size = samp.size))
indices.not_123 = setdiff(indices.not_12, indices.three)
indices.four = sort(sample(indices.not_123, size = samp.size))
indices.not_1234 = setdiff(indices.not_123, indices.four)
indices.five = sort(sample(indices.not_1234, size = samp.size))
indices.not_12345 = setdiff(indices.not_1234, indices.five)
indices.six = sort(sample(indices.not_12345, size = samp.size))
indices.not_123456 = setdiff(indices.not_12345, indices.six)
indices.seven = sort(sample(indices.not_123456, size = samp.size))
indices.eight = setdiff(indices.not_123456, indices.seven)

car_data.1 = car_data[indices.one,]
car_data.2 = car_data[indices.two,]
car_data.3 = car_data[indices.three,]
car_data.4 = car_data[indices.four,]
car_data.5 = car_data[indices.five,]
car_data.6 = car_data[indices.six,]
car_data.7 = car_data[indices.seven,]
car_data.8 = car_data[indices.eight,]

# Select columns that are numeric and save it as a new dataframe
car_new.df1 = car_data[, sapply(car_data, is.numeric)]
str(car_new.df1)

# Remove the columns HP and Price from the dataframe
car_new.df2 = subset(car_data, select = -c(HP, Price))
str(car_new.df2)

# Save the columns Country and Weight as a new datafame
car_new.df3 = car_data[, c("Country", "Weight")]
names(car_new.df3)

# Rename these two columns in the new dataframe
names(car_new.df3)[1] = "New_Country"
names(car_new.df3)[2] = "New_Weight"
names(car_new.df3)
