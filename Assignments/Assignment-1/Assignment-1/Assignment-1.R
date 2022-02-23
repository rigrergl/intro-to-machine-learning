#install.packages('ggplot2')  # use once 
#install.packages('arules')  # use once 

library(ggplot2)
library(arules)

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
ggplot(clean.hepatitis, aes(x = age, y = sgot, color = sex)) + geom_point()
ggplot(clean.hepatitis, aes(sex)) + geom_histogram(stat="count")
ggplot(clean.hepatitis, aes(class)) + geom_histogram(stat="count")

#show the relationship between  a continuous attribute and a discrete attribute (draw at least 4 plots)
ggplot(clean.hepatitis, aes(x = age, y = fatigue, color = class)) + geom_point()
ggplot(clean.hepatitis, aes(x = steroid, y = bilirubin)) + stat_summary(fun.data = mean_sdl, geom = "bar")
ggplot(clean.hepatitis, aes(x = sex, y = bilirubin)) + stat_summary(fun.data = mean_sdl, geom = "bar")
ggplot(clean.hepatitis, aes(x = anorexia, y = sgot)) + stat_summary(fun.data = mean_sdl, geom = "bar")


#2
#Extract continuous features and sex feature from the improved dataset. 
#Compute the averages for them, grouped (aggregated) according to sex. Report your
#findings (at least 3 lines)
continuous.cols <- c("age", "bilirubin", "alk_phosphate", "sgot", "albumin", "protime")
hepatitis.continuous = clean.hepatitis[,continuous.cols]
sex.averages <- aggregate(hepatitis.continuous, by=list(clean.hepatitis$sex), FUN=mean, na.rm = TRUE)

#3
hepatitis.scaled <- scale(hepatitis.continuous)
summary(hepatitis.scaled)
distances <- dist(hepatitis.scaled[1:10,], method="euclidean")
summary(distances)
hist(distances)

#4
sampleids <- sample(1:nrow(clean.hepatitis), 500, replace = TRUE)
sampleids.duplicated <- unique(sampleids[duplicated(sampleids)])
length(sampleids.duplicated)# duplicates in random sample
nrow(clean.hepatitis) # duplicates in original dataset

#6
age.discrete.interval <- discretize(clean.hepatitis$age, method="interval", breaks = 4)
summary(age.discrete.interval)

age.discrete.frequency <- discretize(clean.hepatitis$age, method="frequency", breaks = 4)
summary(age.discrete.frequency)

#7
sample <- clean.hepatitis[, c("age", "alk_phosphate")]

sampleids <- sample(1:nrow(sample), 50, replace = FALSE)
sample <- sample[sampleids,]

cor(sample)
     