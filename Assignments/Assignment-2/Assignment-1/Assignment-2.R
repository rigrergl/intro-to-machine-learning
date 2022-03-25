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

#Create a decision tree with default settings
tree_default <- rpart(class ~ ., data = clean.hepatitis)
tree_default

rpart.plot(tree_default, extra = 2, under = TRUE, varlen = 0, faclen = 0)

pred <- predict(tree_default, clean.hepatitis, type = "class")

#Evaluate Model
default_accuracy <- accuracy(clean.hepatitis$class, pred)
default_accuracy

confusion_table <- table(clean.hepatitis$class, pred)

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
predicted_classes <- pred[1:10]
actual_classes <- clean.hepatitis$class[1:10]

display_df <- data.frame(predicted_classes, actual_classes)
display_df

wrong_pred_count <- nrow(display_df[display_df$predicted_classes != display_df$actual_classes,])
error_rate <- wrong_pred_count / nrow(display_df)
error_rate

#After finding the training error, estimate generalization error for this tree using pessimistic and optimistic approach. 
generalization_df <- data.frame(pred, clean.hepatitis$class)

gen_wrong_pred_count <- nrow(generalization_df[generalization_df$predicted_classes != generalization_df$clean.hepatitis.class,])
gen_error_optimistic <- gen_wrong_pred_count / nrow(generalization_df)
gen_error_optimistic

gen_error_pessimistic <- (gen_wrong_pred_count + nrow(clean.hepatitis) * 0.5) / nrow(generalization_df)
gen_error_pessimistic