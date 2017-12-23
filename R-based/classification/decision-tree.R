# Manish Suthar
# CS 422 
# Illinois Institute of Technology

library(rpart)
library(caret)
library(rpart.plot)
library(psych)
library(ROCR)
library(e1071)
library(plyr)

rm(list=ls())

data <- read.csv("/Users/manishsuthar/Desktop/fall2017/422/labs/lab2/ILPD.csv")

set.seed(100)

#Get the random rows that will be used as training data
training <- sample(1:nrow(data), replace=FALSE, size = 0.6*nrow(data))

# Divide our training and test data
training_data = data[training, ]
test_data = data[-training,]

#Correlation scatter plot to see relationship amongst attributes
pairs.panels(training_data)

"Looking at the correlation matrix, the weakest relationship is the
one that has a coefficient of 0...which is (sgoaa,age)"

"The strongest correlation is a tie between (db,tb) and (sgoaa, sgpaa) 
which both have a correlation coefficient of 0.81"

"The most negatively correlated pair is a tie between (label,db) and (alb,db)
and they have a coefficient of -0.29"

"variables that appear to follow a gaussian distribution are:
  age, tp, alb, and ag"

summary(data)

#scaledData <- scale(data[,3:9], center = T, scale = T)

model <- rpart(label ~ . , method = "class", data= training_data )
rpart.plot(model)

# Use model to predict test data and verify accuracy of model
predicted_label <- predict(model, test_data[,1:10], type="class")

# Confusion Matrix 
matrix_before_pruning <- confusionMatrix(predicted_label, test_data[,11])

plotcp(model)

# Use Cross Validation to get the best place to prune the tree
min_cp <- model$cptable[which.min(model$cptable[,"xerror"]),"CP"]

pruned_tree <- prune(model, cp = min_cp)

# Predict same test data with new model
predicted_label_post_prune <- predict(object = pruned_tree, newdata = test_data[,1:10], type = "class")

matrix_after_pruning <- confusionMatrix(predicted_label_post_prune, reference = test_data[,11])
matrix_after_pruning
plotcp(x = pruned_tree)
printcp(x = pruned_tree)
rpart.plot(x = pruned_tree, fallen.leaves = TRUE)

# test model 1
test_model1 <- rpart(label ~ age + sex + tb + aap + sgoaa + tp + alb + ag 
                     , method = "class", data = training_data)
pred_test_model1 <- predict(test_model1, newdata = test_data[,1:10], type = "class")

rpart.plot(test_model1)

confusion_matrix_tmodel1 <- confusionMatrix(data = pred_test_model1, reference = test_data[,11])
confusion_matrix_tmodel1

# test model 2

test_model2 <- rpart(label ~ age + sex + db + aap + sgpaa + tp + alb 
                     , method = "class", data = training_data)

pred_test_model2 <- predict(test_model2, newdata = test_data[,1:10], type = "class")
confusion_matrix_tmodel2 <- confusionMatrix(data = pred_test_model2, reference = test_data[,11])
rpart.plot(test_model2)
confusion_matrix_tmodel2

# test model 3
test_model3 <- rpart(label ~ age + sex + tb + aap + sgpaa + tp + alb
                     , method = "class", data = training_data)

pred_test_model3 <- predict(test_model3, newdata = test_data[,1:10], type = "class")
confusion_matrix_tmodel3 <- confusionMatrix(data = pred_test_model3, reference = test_data[,11])
rpart.plot(test_model3)
confusion_matrix_tmodel3

# test model 4
test_model4 <- rpart(label ~ age + sex + db + aap + sgpaa + ag 
                     , method = "class", data = training_data)

pred_test_model4 <- predict(test_model4, newdata = test_data[,1:10], type = "class")
confusion_matrix_tmodel4 <- confusionMatrix(data = pred_test_model4, reference = test_data[,11])
rpart.plot(test_model4)
confusion_matrix_tmodel4

# Roc Curve for original model
pred.rocr <- predict(object = model, newdata = test_data, type = "prob")
f.pred <- prediction(pred.rocr[,2], test_data$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]

# Roc Curve for test_model1
# 70.512% accuracy
# AUC: 0.6762445
pred.rocr <- predict(object = test_model1, newdata = test_data, type = "prob")
f.pred <- prediction(pred.rocr[,2], test_data$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]

# Roc Curve for test_model2
# 70.085% accuracy
# AUC: 0.6751274
pred.rocr <- predict(object = test_model2, newdata = test_data, type = "prob")
f.pred <- prediction(pred.rocr[,2], test_data$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]

# Roc Curve for test_model3
# 70.94% accuracy
# AUC: 0.6411654
pred.rocr <- predict(object = test_model3, newdata = test_data, type = "prob")
f.pred <- prediction(pred.rocr[,2], test_data$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]

# Roc Curve for test_model4
# 64% accuracy
# 0.7591384
pred.rocr <- predict(object = test_model4, newdata = test_data, type = "prob")
f.pred <- prediction(pred.rocr[,2], test_data$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
auc@y.values[[1]]

original_data <- read.csv("/Users/manishsuthar/Desktop/fall2017/422/labs/lab2/ILPD-original.csv")

# Add column names to data frame
names(original_data)[1] <- "age"
names(original_data)[2] <- "sex"
names(original_data)[3] <- "tb"
names(original_data)[4] <- "db"
names(original_data)[5] <- "aap"
names(original_data)[6] <- "sgpaa"
names(original_data)[7] <- "sgoaa"
names(original_data)[8] <- "tp"
names(original_data)[9] <- "alb"
names(original_data)[10] <- "ag"
names(original_data)[11] <- "label"

# get number of missing columns
num_missing <- sum(is.na(original_data[,1:11]))

# HERE WE WILL FIND OUT ALL THE MISSING INSTANCES AND ALSO FIGURE OUT 
# HOW THEY WERE IMPUTED WITH A SERIES OF LOOPS AND R COMMANDS
rows_with_na <- c()
i = 0
for (val in original_data$ag) {
    i = i + 1
    if(is.na(val)) {
      #print(val)
      print(original_data[i,])
      print(row.names(original_data[i,]))
      rows_with_na <- c(rows_with_na, as.numeric(row.names(original_data[i,])) )
      
    } 
  else {
    #print(val)
  }
}

#This frams contains all the missing values 
missing_values_frame <- original_data[rows_with_na,]

# This frame contains the same rows as the missing values frame
# but is taken from the provided data set so we can analyze how
# the missing values were imputed
imputed_frame <- data[rows_with_na + 1,]

median(data$ag)

"The data was filled in using the median for the ag column"