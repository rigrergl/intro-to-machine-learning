#install.packages('ggplot2')  # use once 

library(ggplot2)

hepatitis <- read.table("hepatitis_csv.csv", sep=",", header=TRUE, na.strings=c("", "NA"))
hepatitis

#Are there duplicate entries? No
duplicates <- duplicated(hepatitis)
duplicates

#return a dataframe indicating which cases are complete, i.e., have no missing values
clean.hepatitis <- hepatitis[complete.cases(hepatitis),]
summary(clean.hepatitis)

#Display the distribution of attributes (at least 5 attributes)
hist(clean.hepatitis$bilirubin)
boxplot(clean.hepatitis$age, data = clean.hepatitis, horizontal = TRUE, main = "Age Distribution")
ggplot(clean.hepatitis, aes(x = age, y = sgot ,color = sex)) + geom_point()
ggplot(clean.hepatitis, aes(sex)) + geom_histogram(stat="count")
ggplot(clean.hepatitis, aes(class)) + geom_histogram(stat="count")
