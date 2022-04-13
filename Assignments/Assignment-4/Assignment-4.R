#install.packages('boot', dep=TRUE)

library(boot)
library(rpart)

#####################Question 6#################################
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