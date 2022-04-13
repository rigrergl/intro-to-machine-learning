#install.packages('rpart')
#install.packages('caret')

library(caret)
library(rpart)

#Function for accuracy
accuracy <- function(truth, prediction) {
  tbl <- table(truth, prediction)
  sum(diag(tbl))/sum(tbl)
}

#Import dataset
hepatitis <- read.table("hepatitis_csv.csv", sep=",", header=TRUE, na.strings=c("", "NA"))
hepatitis

#Clean dataset (remove duplicates and missing values)
duplicates <- duplicated(hepatitis)
duplicates # there are no duplicates

clean.hepatitis <- hepatitis[complete.cases(hepatitis),]
summary(clean.hepatitis)

#Translate all the TRUE/FALSE values into factors (nominal)
for (i in c(3:13, 19:20)) {
  clean.hepatitis[[i]] <- as.factor(clean.hepatitis[[i]])
}

#####################Question 6#################################
#create folds
index <- 1:nrow(clean.hepatitis)
index <- sample(index) #shuffle index

fold <- rep(1:10, each=nrow(clean.hepatitis)/10)[1:nrow(clean.hepatitis)]
fold

folds <- split(index, fold) ## create list with indices for each fold
folds

#Do each fold
cps = c(0.003197442, 0.006705747, 0.036903810, 0.064481748, 0.128497202)
cp_accs <- vector(mode="numeric")
cp_errors <- vector(mode="numeric")

for(i_cp in 1:length(cps)) {
  accs <- vector(mode="numeric")
  for(i in 1:length(folds)){
    tree <- rpart(class~ ., data=clean.hepatitis[-folds[[i]],], control=rpart.control(minsplit=2, cp=cps[i_cp]))
    accs[i] <- accuracy(clean.hepatitis[folds[[i]],]$class, predict(tree, clean.hepatitis[folds[[i]],], type="class"))
  }
  
  cp_accs[i_cp] = mean(accs)
  cp_errors[i_cp] = 1 - cp_accs[i_cp]
}

cp_accs
cp_errors

#####################Question 7#################################
library(caret)
fit <- train(class ~ ., data = clean.hepatitis, method = "rpart",
             control=rpart.control(minsplit=2),
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=5)

fit

varImp(fit, compete = FALSE)
dotPlot(varImp(fit, compete=FALSE))

varImp(fit, compete = TRUE)
dotPlot(varImp(fit, compete=TRUE))

#####################Question 8#################################
#Most important attributes: protime, histology, and albumin
hep.scaled <- clean.hepatitis[, 17:20]

# Standardize data (Z-score)
hep.scaled[,1:2] <- scale(hep.scaled[,1:2])

#####################Question 9#################################
knnFit <- train(class ~ ., method = "knn", data = hep.scaled,
                tuneLength = 5, tuneGrid=data.frame(k=1:10),
                trControl=trainControl(
                  method = "cv"))
knnFit
plot(knnFit)