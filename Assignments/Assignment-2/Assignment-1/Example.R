
#Prepare Zoo Data Set
#Prepare Zoo Data Set
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


?rpart
data(Zoo, package="mlbench")
head(Zoo)

#Get summary statistics
summary(Zoo)

typeof(Zoo$hair)

#translate all the TRUE/FALSE values into factors (nominal)
for(i in c(1:12, 14:16))
  Zoo[[i]] <- as.factor(Zoo[[i]])

summary(Zoo)

#A First Decision Tree
#Recursive Partitioning (similar to CART) uses the Gini index to make splitting decisions and early stopping (pre-pruning).
nrow(Zoo)

unique(Zoo$type)

#frequency of each class
round (nrow(Zoo[which (Zoo$type=="mammal"),]) /nrow(Zoo), 2 )
round (nrow(Zoo[which (Zoo$type=="bird"),]) /nrow(Zoo) ,2)
round (nrow(Zoo[which (Zoo$type=="reptile"),]) /nrow(Zoo) ,2)
round (nrow(Zoo[which (Zoo$type=="fish"),]) /nrow(Zoo) ,2)
round (nrow(Zoo[which (Zoo$type=="amphibian"),]) /nrow(Zoo) ,3)
round (nrow(Zoo[which (Zoo$type=="insect"),]) /nrow(Zoo) ,3)
round (nrow(Zoo[which (Zoo$type=="mollusc.et.al"),]) /nrow(Zoo) ,3)


#Create Tree With Default Settings (uses pre-pruning)
tree_default <- rpart(type ~ ., data = Zoo)
tree_default

rpart.plot(tree_default, extra = 2, under = TRUE, varlen=0, faclen=0)


#extra=2: display the classification rate at the node, expressed as the number of correct classifications
#and the number of observations in the node.
#under=true ; to put the text under the box.
# varlen; Length of variable names in text at the splits , 0 meaning display the full variable names.
# faclen;Length of factor level names in splits. Default 0, meaning display the full factor names

# predict a class (type of animal) for the objects of the zoo dataset based on the tree-deafult
pred <- predict(tree_default, Zoo, type="class")
head(pred)


#table uses the cross-classifying factors to build a contingency table of the counts at each combination of factor levels.
# calculate accuracy and training error on tree with pre-pruning (tree-deafult)
confusion_table <- table(Zoo$type, pred)
confusion_table

correct <- sum(diag(confusion_table))
correct

error <- sum(confusion_table)-correct
error

accuracy <- correct / (correct+error)
accuracy

error<- error / (correct+error) 
error

#Create a Full Tree
#To create a full tree, we set the complexity parameter cp to 0 (split even if it does not improve the tree) and we set the minimum number of observations in a node needed to split to the smallest value of 2 (see: ?rpart.control). Note: full trees overfit the training data!

tree_full <- rpart(type ~., data=Zoo, control=rpart.control(minsplit=2, cp=0))
rpart.plot(tree_full, extra = 2, under = TRUE,  varlen=0, faclen=0)


# calculate accuracy and training error on the full tree
pred_full <- predict(tree_full, Zoo, type="class")
head(pred_full)

# calculate accuracy and training error on tree with pre-pruning (tree-deafult)
confusion_table_full <- table(Zoo$type, pred_full)
confusion_table_full

correct_full <- sum(diag(confusion_table_full))
correct_full


error_full <- sum(confusion_table_full)-correct_full
error_full

accuracy_full <- correct_full / (correct_full+error_full)
accuracy_full

error_full<- error_full / (correct_full+error_full) 
error_full


# or Use a function for accuracy

accuracy <- function(truth, prediction) {
  tbl <- table(truth, prediction)
  sum(diag(tbl))/sum(tbl)
}

accuracy(Zoo$type, pred)

accuracy(Zoo$type, predict(tree_full, Zoo, type="class"))


#Get a confusion table with more statistics (using caret)
library(caret)
confusionMatrix(data = pred, reference = Zoo$type)


#Make Predictions for New Data
#Make up my own animal: A lion with feathered wings

my_animal <- data.frame(hair = TRUE, feathers = TRUE, eggs = FALSE,
                        milk = TRUE, airborne = TRUE, aquatic = FALSE, predator = TRUE,
                        toothed = TRUE, backbone = TRUE, breathes = TRUE, venomous = FALSE,
                        fins = FALSE, legs = 4, tail = TRUE, domestic = FALSE,
                        catsize = FALSE, type = NA)

#Fix columns to be factors like in the training set.
for(i in c(1:12, 14:16)) my_animal[[i]] <- factor(my_animal[[i]],
                                                  levels = c(TRUE, FALSE))

my_animal


#Make a prediction using the default tree
predict(tree_default , my_animal, type = "class")


#Make a prediction using the full tree
predict(tree_full , my_animal, type = "class")


#Model Evaluation
#Pure R Implementation
#Holdout Sample

#Use a simple split into 2/3 training and 1/3 testing data. Find the size of the training set.
n_train <- as.integer(nrow(Zoo)*.66)
n_train


#Randomly choose the rows of the training examples.
train_id <- sample(1:nrow(Zoo), n_train)
head(train_id)

#Split the data
train <- Zoo[train_id,]
test <- Zoo[-train_id, colnames(Zoo) != "type"]
test_type <- Zoo[-train_id, "type"]

# create a tree using the training set (2/3 of the zoo data set )
tree <- rpart(type ~., data=train,control=rpart.control(minsplit=2))

#Training error (based on the above tree) on training set
accuracy(train$type, predict(tree, train, type="class"))

1- accuracy(train$type, predict(tree, train, type="class"))

#Generalization error (based on the above tree) on  testing set (1/3 of the zoo data set )
accuracy(test_type, predict(tree, test, type="class"))

1- accuracy(test_type, predict(tree, test, type="class"))

#10-Fold Cross Validation
index <- 1:nrow(Zoo)
index <- sample(index) ### shuffle index


fold <- rep(1:10, each=nrow(Zoo)/10)[1:nrow(Zoo)]
fold

folds <- split(index, fold) ### create list with indices for each fold
folds

#Do each fold
accs <- vector(mode="numeric")
for(i in 1:length(folds)) {
  tree <- rpart(type ~., data=Zoo[-folds[[i]],], control=rpart.control(minsplit=2))
  accs[i] <- accuracy(Zoo[folds[[i]],]$type, predict(tree, Zoo[folds[[i]],], type="class"))
}
accs

#Report the average
mean(accs)

#Caret: For Easier Model Building and Evaluation
#Enable multi-core
library(doParallel)

registerDoParallel()

#caret packages training and testing into a single function called train().
#It internally splits the data into training and testing sets and thus will provide you with generalization error estimates. 
#trainControl is used to choose how testing is performed.

#Train also tries to tune extra parameters by trying different values. For rpart, train tries to tune the cp parameter (tree complexity) using accuracy to choose
#the best model. I set minsplit to 2 since we have not much data. Note: Parameters used for tuning (in this case cp) need to be set using a data.frame in 
#the argument tuneGrid! Setting it in control will be ignored.

library(caret)
fit <- train(type ~ ., data = Zoo , method = "rpart",
             control=rpart.control(minsplit=2),
             trControl = trainControl(method = "cv", number = 10),
             tuneLength=5)
fit

fit$bestTune

#Note: Train has built 10 trees. Accuracy and kappa for each tree/test fold can be obtained.
fit$resample

sum(fit$resample$Accuracy)/10

sum(fit$resample$Kappa)/10

#A model using the best tuning parameters and using all the data is available as fit$finalModel.
fit$finalModel

rpart.plot(fit$finalModel, extra = 2, under = TRUE,  varlen=0, faclen=0)

#Here is the variable importance without competing splits
varImp(fit, compete = FALSE)

dotPlot(varImp(fit, compete=FALSE))

#Repeated Bootstrap Sampling
#An alternative to CV is repeated bootstrap sampling. It will give you very similar estimates.

fit <- train(type ~ ., data = Zoo, method = "rpart",
             control=rpart.control(minsplit=2),
             trControl = trainControl(method = "boot", number = 10),
             tuneLength=5)
fit











































































