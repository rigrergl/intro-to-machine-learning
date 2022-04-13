#install.packages('mlbench')
#install.packages('rpart')
#install.packages('rpart.plot')
#install.packages('caret')
#install.packages('lattice')
#install.packages('e1071')
#install.packages('doParallel')
#install.packages('foreach')
#install.packages('iterators')
#install.packages('parallel')

library(mlbench)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(lattice)
library(e1071)
library(caret)
library(parallel)
library(iterators)
library(foreach)
library(doParallel)

#4
#Repeated Bootstrap Sampling
#An alternative to CV is repeated bootstrap sampling. It will give you very similar estimates.

person_id <- 1:10
home_owner <- c (TRUE, 
                 FALSE,
                 TRUE,
                 FALSE,
                 TRUE,
                 FALSE,
                 FALSE,
                 FALSE,
                 FALSE,
                 TRUE)

refund <- c(TRUE,
            TRUE,
            FALSE,
            FALSE,
            TRUE,
            TRUE,
            FALSE,
            TRUE,
            TRUE,
            FALSE)

df <- data.frame(person_id, home_owner, refund)

for(i in c(2:3))
  df[[i]] <- as.factor(df[[i]])

fit <- train(refund ~ ., data = df, method = "rpart",
             control=rpart.control(minsplit=2),
             trControl = trainControl(method = "boot", number = 10),
             tuneLength=5)
fit