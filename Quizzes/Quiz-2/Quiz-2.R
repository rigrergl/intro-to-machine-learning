# Name: Rigre Garciandia
# NetID: RRG190004
# CS 4375.002

#install.packages('ggplot2')  # use once 
#install.packages('sampling')  # use once 
#install.packages('scatterplot3d')  # use once 
#install.packages('arules')  # use once 
#install.packages('Matrix')    # use once 

library(ggplot2)
library(sampling)
library(scatterplot3d)
library(Matrix)
library(arules)

#create a data frame
student.id <- c(1, 2, 3, 4)

student.name <- c("John Smith",
                  "Nagisa Shiota",
                  "Sokka Boomerang",
                  "Hermione Granger")

grade1 <- c(20, 20, 50, 99)
grade2 <- c(20, 87, 97, 98)
grade3 <- c(50, 89, 20, 96)

sex <- c("M", "M", "M", "F")

study.hours.per.week <- c(6, 7, 2, 8)

df <- data.frame(student.id, student.name, grade1, grade2, grade3, sex, study.hours.per.week)

#calculate GPA
df$gpa = rowMeans(df[,c(3,4,5)])

#discretize grades
df$gpa.discrete <- cut(df$gpa, breaks=3,
                 labels=c("bad", "good", "very good"), ordered=TRUE)

# draw 3 plots to show the distributions of the grades
plot(df$gpa, 1:4, ylab="index")
hist(df$gpa)
barplot(table(df$gpa.discrete), xlab="gpa", ylab="count" )

# draw a plot to show the distribution of a continuous attribute in the original dataset
hist(df$grade1)

# draw a plot to show the distribution between a continous attribute and
# a discrete attribute in the original dataset
stats <- aggregate(study.hours.per.week ~ gpa.discrete, data = df, FUN = mean)
ggplot(stats, aes(x=gpa.discrete, y=study.hours.per.week)) + geom_bar(stat='identity')

