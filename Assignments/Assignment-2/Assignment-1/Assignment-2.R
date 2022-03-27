#install.packages('rpart.plot')

library(rpart)
library(rpart.plot)

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

#Function for accuracy
accuracy <- function(truth, prediction) {
  tbl <- table(truth, prediction)
  sum(diag(tbl))/sum(tbl)
}

error <- function(truth, prediction) {
  1 - accuracy(truth, prediction)
}

#Create a decision tree with default settings
tree_default <- rpart(class ~ ., data = clean.hepatitis)
tree_default

rpart.plot(tree_default, extra = 2, under = TRUE, varlen = 0, faclen = 0)

pred_default <- predict(tree_default, clean.hepatitis[, -20], type = "class")

#Evaluate Model
default_accuracy <- accuracy(clean.hepatitis$class, pred_default)

confusion_table <- table(clean.hepatitis$class, pred_default)

tp <- confusion_table["live", "live"]
tn <- confusion_table["die", "die"]
fn <- confusion_table["live", "die"]
fp <- confusion_table["die", "live"]

tpr <- tp / (tp + fn) 
tpr
 
fpr <- fp / (fp + tn)
fpr 
 
tnr <- tn / (tn + fp)
tnr 
 
fnr <- fn / (fn + tp)
fnr

#Display the actual class label and the predicted class label for the first ten objects of the improved Hepatitis data set. Find the error rate for these ten objects. 
confusion_table_head <- table(clean.hepatitis$class[1:10], pred_default[1:10])
wrong_pred_count_head <- confusion_table_head["die", "live"] + confusion_table["live", "die"]
error_rate <- wrong_pred_count_head / 10
error_rate

#After finding the training error, estimate generalization error for this tree using pessimistic and optimistic approach. 
wrong_pred_count_default <- confusion_table["die", "live"] + confusion_table["live", "die"]

gen_error_optimistic_default <- wrong_pred_count_default / nrow(clean.hepatitis)
gen_error_optimistic_default

gen_error_pessimistic_default <- (wrong_pred_count_default + nrow(clean.hepatitis) * 0.5) / nrow(clean.hepatitis)
gen_error_pessimistic_default

##################### Part 2 ##########################

#Create a fully grown decision tree
tree_full <- rpart(class ~., data = clean.hepatitis, control = rpart.control(minsplit = 2, cp = 0))
rpart.plot(tree_full, extra = 2, under = TRUE,  varlen=0, faclen=0)

pred_full <- predict(tree_full, clean.hepatitis, type = "class")

#Evaluate Model
full_accuracy <- accuracy(clean.hepatitis$class, pred_full)
full_accuracy

confusion_table_full_tree <- table(clean.hepatitis$class, pred_full)

tp <- confusion_table_full_tree["live", "live"]
tn <- confusion_table_full_tree["die", "die"]
fn <- confusion_table_full_tree["live", "die"]
fp <- confusion_table_full_tree["die", "live"]

tpr <- tp / (tp + fn) 
tpr

fpr <- fp / (fp + tn)
fpr 

tnr <- tn / (tn + fp)
tnr 

fnr <- fn / (fn + tp)
fnr

actual_classes <- clean.hepatitis$class

comparison_df <- data.frame(pred_full, actual_classes)
comparison_df

wrong_pred_count <- nrow(comparison_df[comparison_df$predicted_classes != comparison_df$actual_classes,])
error_rate_full <- wrong_pred_count / nrow(comparison_df)
error_rate_full

#After finding the training error, estimate generalization error for this tree using pessimistic and optimistic approach. 
wrong_pred_count_full <- confusion_table_full_tree["die", "live"] + confusion_table_full_tree["live", "die"]

gen_error_optimistic_full <- wrong_pred_count_full / nrow(clean.hepatitis)
gen_error_optimistic_full

gen_error_pessimistic_full <- (wrong_pred_count_full + nrow(clean.hepatitis) * 0.5) / nrow(clean.hepatitis)
gen_error_pessimistic_full

########################### Part 3 ########################### 
#Construct a decision tree for the improved “Hepatitis” data set to illustrate under-fitting. 

tree_under_fitted <- rpart(class ~., data = clean.hepatitis, control = rpart.control(minsplit = 2, cp = .4))
rpart.plot(tree_under_fitted, extra = 2, under = TRUE,  varlen=0, faclen=0)

pred_under_fitted <- predict(tree_under_fitted, clean.hepatitis, type = "class")
accuracy(clean.hepatitis$class, pred_under_fitted)

########################### Part 4 ########################### 
#Create a tree with 2/3 training 1/3 testing
n_train <- as.integer(nrow(clean.hepatitis) * .66)
n_train

#Randomly choose the rows 
train_id <- sample(1:nrow(clean.hepatitis), n_train)

train <- clean.hepatitis[train_id,]
test <- clean.hepatitis[-train_id, colnames(clean.hepatitis) != "class"]
test_class <- clean.hepatitis[-train_id, "class"]

tree_1 <- rpart(class ~ ., data = train, control = rpart.control((minsplit = 2)))
rpart.plot(tree_1, extra = 2, under = TRUE,  varlen=0, faclen=0)

training_error_1 <- error(train$class, predict(tree_1, train, type = "class"))
training_error_1

test_error_1 <- error(test_class, predict(tree_1, test, type = "class"))
test_error_1

#Then, randomly select 15 objects of the improved “Hepatitis” data set as the training dataset and consider the remaining objects as the test dataset
train_id_2 <- sample(1:nrow(clean.hepatitis), 15)

train_2 <- clean.hepatitis[train_id_2,]
test_2 <- clean.hepatitis[-train_id_2, colnames(clean.hepatitis) != "class"]
test_class_2 <- clean.hepatitis[-train_id_2, "class"]

tree_2 <- rpart(class ~., data = train_2, control = rpart.control(minsplit = 2))
rpart.plot(tree_2, extra = 2, under = TRUE,  varlen=0, faclen=0)

training_error_2 <- error(train_2$class, predict(tree_2, train_2, type = "class"))
training_error_2

test_error_2 <- error(test_class_2, predict(tree_2, test_2, type = "class"))
test_error_2