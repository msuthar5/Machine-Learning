# Manish Suthar
# CS 422 
# Illinois Institute of Technology

library(caret)
library(ROCR)
library(e1071)
library(arules)
library(rpart)
library(rpart.plot)
library(cluster)
library(factoextra)


rm(list=ls())

ilpd_data <- read.csv("/Users/manishsuthar/Desktop/fall2017/422/labs/lab3/ILPD.csv")

# Split the data into 60% training and 40% test
set.seed(100)
training <- sample(1:nrow(ilpd_data), replace = FALSE, size= 0.6*nrow(ilpd_data) )
training_data <- ilpd_data[training, ]
test_data <- ilpd_data[-training,]

#Split the training and test data for the bayes model
index <- sample(1:nrow(ilpd_data), size=0.4*nrow(ilpd_data))
test <- ilpd_data[index, ]
train <- ilpd_data[-index, ]

# Originally I said test_model4 was the best model but 
# after doing some more research I believe that test_model3
# is better. The reason I was confused is because model4 
# had the lowest accuracy but had the largest AUC so I 
# based it off of AUC. Now I am basing it off of overally accuracy

# Here is the model for test_model3 Note it is re-named to 
# best_model 

# test model 3
best_model <- rpart(label ~ age + sex + tb + aap + sgpaa + tp + alb
                     , method = "class", data = training_data)

rpart.plot(best_model)
pred_best_model <- predict(best_model, newdata = test_data[,1:10], type = "class")
confusion_matrix_best_model <- confusionMatrix(data = pred_best_model, reference = test_data[,11])

# Roc Curve for best_model
# 70.09% accuracy
# AUC: 0.6737421
pred.rocr <- predict(object = best_model, newdata = test_data, type = "prob")
f.pred <- prediction(pred.rocr[,2], test_data$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
best_model_auc <-auc@y.values[[1]]

# Bayes model on same attributes
bayes_model <- naiveBayes(as.factor(label) ~ age + sex + tb + aap + sgpaa + tp + alb, 
                          method ="class", data=training_data)

bayes_pred <- predict(bayes_model, test_data, type="class")

bayes_confusion_matrix = confusionMatrix(bayes_pred, test_data[,11], positive='1')
bayes_confusion_matrix

pred.rocr <- predict(object = bayes_model, newdata = test_data, type = "raw")
f.pred <- prediction(pred.rocr[,2], test_data$label)
f.perf <- performance(f.pred, "tpr", "fpr")
plot(f.perf, colorize=T, lwd=3)
abline(0,1)
auc <- performance(f.pred, measure = "auc")
bayes_auc <- auc@y.values[[1]]

# Compare the AUC from the ROC curve from our naive_bayes model and 
# original best_model from hw 2
bayes_auc
best_model_auc

"The naive_bayes based model had a higher AUC than that of our best model
Therefore, THE NAIVE_BAYES model had a larger AUC and is therefore better
based on that metric alone"

#----------------------------- Market Basket Analysis -------------------------------

transactions <- read.transactions("/Users/manishsuthar/Desktop/fall2017/422/labs/lab3/groceries.csv", sep = ",")
inspect(transactions[1:5])

"The most frequently bought item: whole milk --> 2513"
summary(transactions)

rules <- apriori(transactions)
summary(rules)
inspect(sort(rules, by='count', increasing = T))

# Find all items with min_sup = 0.1
itemFrequencyPlot(transactions, support = 0.1)
image(transactions)

# Rules Generated at support = 0.003
minsupport_rules <- apriori(transactions, parameter = list(support=0.003))
inspect(rules)

rules_1 <- apriori(transactions, 
                   parameter = list(support = 0.0012))

# Top 5 rules by support are:
# (1) {citrus fruit, root vegetables, tropical fruit, whole milk} -> {other vegetables} 0.003152008
# (2) {curd, domestic eggs, other vegetables} -> {whole milk} 0.002846975
# (3) {curd, hamburger meat} -> {whole milk} 0.002541942
# (4) {herbs, rolls/buns} -> {whole milk} 0.002440264
# (5) {herbs, tropical fruit} -> {whole milk} 0.002338587
inspect(sort(rules_1, by='support', decreasing = T))


# Top 5 rules by confidence are:
# (1) {rice,sugar} -> {whole milk} 1.0
# (2) {flour, root vegetables, whipped/sour cream} -> {whole milk} 1.0
# (3) {oil, other vegetables, root vegetables, yogurt} -> {whole milk} 1.0
# (4) {butter, domestic eggs, other vegetables, whipped/sour cream} -> {whole milk} 1.0
# (5) {citrus fruit, root vegtables, tropical fruit, whipped/sour cream} -> {other vegetables} 1.0
inspect(sort(rules_1, by='confidence', decreasing = F))

# Top 5 rules by lift are:
# (1) {liquor, red/blush wine} -> {bottled beer} 11.235269
# (2) {oil, other vegetables, tropical fruit, whole milk} -> {root vegetables} 7.951182
# (3) {other vegetables, rice, whole milk, yogurt} -> {root vegetables} 7.951182
# (4) {butter, curd, tropical fruit, whole milk} -> {yogurt} 6.144315
# (5) {pip fruit, sausage,slived cheese} -> {yogurt} 6.144315
inspect(sort(rules_1, by='lift', decreasing = F))


# Broaden our search of rules by lowering support to 0.001
rules_2 <- apriori(transactions, 
                   parameter = list(supp = 0.009))

# Bottom 5 rules by support are:
# (1) {citrus fruit, root vegetables, whipped/sour cream, whole milk, yogurt} -> {other vegetables} 
# (2) {citrus fruit, other vegetables, root vegetables, whipped/sour cream, yogurt} -> {whole milk} 
# (3) {butter, domestic eggs, tropical fruit, whole milk, yogurt} -> {other vegetables} 
# (4) {butter, domestic eggs, other vegetables, tropical fruit, yogurt} -> {whole milk} 
# (5) {oil, other vegetables, tropical fruit, whole milk, yogurt} -> {root vegetables} 
inspect(sort(rules_2, by='support', decreasing = T))

# Bottom 5 rules by confidence are:
# (1) {rolls/buns, root vegetables, tropical fruit, whipped/sour cream} -> {other vegetables} 
# (2) {citrus fruit, tropical fruit, whipped/sour cream, yogurt} -> {whole milk} 
# (3) {citrus fruit, sausage, whipped/sour cream, whole milk} -> {other vegetables} 
# (4) {cirtus fruit, other vegetables, sausage, whipped/sour cream} -> {whole milk} 
# (5) {pip fruit, tropical fruit, whipped/sour cream, whole milk} -> {other vegetables} 
inspect(sort(rules_2, by='confidence', decreasing = T))

# Bottom 5 rules by lift are:
# (1) {citrus fruit, tropical fruit, whipped/sour cream, yogurt} -> {whole milk} 3.130919
# (2) {cirtus fruit, other vegetables, sausage, whipped/sour cream} -> {whole milk} 3.130919
# (3) {butter, other vegetables, rolls/buns, whipped/sour cream} -> {whole milk} 3.130919
# (4) {butter, tropical fruit, whipped/sour cream, yogurt} -> {whole milk} 3.130919
# (5) {butter, curd, tropical fruit, yogurt} -> {whole milk} 3.130919
inspect(sort(rules_2, by='lift', decreasing = T))

#-----------------------------Clustering---------------------------------

clust_df <- read.csv('https://people.sc.fsu.edu/~jburkardt/datasets/hartigan/file19.txt', header = F, sep = "", comment.char = "#")

# Cleaning the Data frame
elim_rows = c(1:4)
clust_df <- clust_df[-elim_rows,]
colnames(clust_df)<- c('Name', 'I','i','C','c','P','p','M','m')
summary(clust_df)

"One attribute that we would remove is the name attribute because we can only deal with categorial attributes"

"We do not need to standardize the attributes they are already in 1-10 scale"
#Convert the df to a csv_file
csv_file <- write.csv(clust_df, file = "/Users/manishsuthar/Desktop/fall2017/422/labs/lab3/cleaned_data.csv", row.names = F)

cleaned_clust_df <- read.csv("/Users/manishsuthar/Desktop/fall2017/422/labs/lab3/cleaned_data.csv")

numeric_df <- cleaned_clust_df[,-1]


# Based on the wss graph, the optiminal number of clusters is 9
fviz_nbclust(numeric_df, kmeans, method="wss")

# Based on the silhouette graph the best number of clusters is 8
fviz_nbclust(numeric_df, kmeans, method="silhouette")

k <- kmeans(numeric_df, centers = 5, nstart = 40)
fviz_cluster(k, data = numeric_df)

# Loop to see how many obervations are in each cluster
i = 1
while (i <= 5) {
  cat(sprintf("number of observations in cluster %d --> %d \n", i, length(which(k$cluster == i))))
  cat(sprintf("%s", which(k$cluster == i)))
  cat(sprintf("\n"))
  i = i+ 1
} 

# SSE per cluster: 23.47368 10.72727 41.33333  0.00000 42.75000
# Total SSE: 118.28425

clust1_obj <- cleaned_clust_df[c(13:31),1]
clust2_obj <- cleaned_clust_df[c(9 ,10, 58, 59, 60, 61, 62, 63, 64, 65, 66),1]
clust3_obj <- cleaned_clust_df[c(1, 2, 3, 4, 5, 6, 7, 8, 11, 32, 33, 34, 35, 37, 57),]
clust4_obj <- cleaned_clust_df[12,1]
clust5_obj <- cleaned_clust_df[c(36, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56),1]

# As can be seen from the below output, cluster 1 objects consist of all small animals. This makes sense
# becuase they all have smaller bodies and therefore smaller teeth and similar values for the attributes.
clust1_obj

# As can be seen from the below output, cluster 2 consists of larger animals such as sheep, reindeer
# antelopes and bison which are all large, 4 legged, and have similar values for attributes describing 
# their teeth. I'm no scientiest in the field of mammals but it is safe to presume that they also
# eat similiar food so they should all have similiar characteristics of their teeth to be able to consume
# common foods.
clust2_obj

# As can be seen from the below output, cluster 3 consists of a slight mix of larger animals and a couple smaller animals
# This also makes sense however as despite their large body size, they all have similar values for their
# attributes. For example,despite bats being small, they have slighter larger teeth than other small animals such as rabbits
# and therefore that is why the Brown Bat and Pigmy Bat are grouped with larger mammals such as a fox.
# To focus on the similarity of this cluster, cayote, wolf, fox, and bears are in this cluster and it is obvious
# that these animals are similiar in size and food they eat.
clust3_obj

# This cluster, while it only contains one object, Armadillo, this makes sense because the armadillo
# because it does not have any teeth another than molars.
clust4_obj

# This cluster makes perfect sense. It consists of mostly animals that live in the water/spend majority 
# of time in the water. Therefore, these animals eat similar cusine. These animals tend to have larger front
# teeth.
clust5_obj
clust5_obj