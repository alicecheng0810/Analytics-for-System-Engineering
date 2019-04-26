######################################
#In-class exercise
#####################################
#Use IRis data set to predict Species with naiveBayes() function
# Use Iris data set
iris<-data.frame(iris)
head(iris)
iris$Species<-factor(iris$Species)
is.factor(iris$Species)

set.seed(10)

#Q1)randomly sample 120 cases out of 150 without replacement for tranning set, remaining will be test
#divide iris data accordingly to train and test sets
#create separet class vectors fr response for training  and test 

dim(iris)
smp_size <- floor(0.8*nrow(iris))
train_ind <- sample(seq_len(nrow(iris)), size = smp_size)
train <- iris[train_ind,]
test <- iris[-train_ind,]
train_variable <- train[,-5]
train_response <- train[5]
test_variable < - test[,-5]
test_variable <- test[5]

#Q2) Use naiveBayes() to build a model for classifying Species

#Fitting the Naive Bayes model
Naive_Bayes_Model=naiveBayes(Species ~., data=train)
#What does the model say? Print the model summary
Naive_Bayes_Model

#Q3) Predict specied for test set

#Prediction on the test dataset
NB_Predictions=predict(Naive_Bayes_Model,newdata=test,type="class")

#Q4) create confusion matrix and calculate test missclassification error and accuracy
#Confusion matrix to check accuracy
confusion.matrix=table(NB_Predictions,test$Species)
confusion.matrix 
# error rate: 0.033333
1/(8+12+1+9) 
# accuracy rate : 0.96666
1- (1/(8+12+1+9) )

#Q5) Check the independence assumption for predictors that have the same class label
#Does conditional independence assumption hold?
#Correlation for trainning set: 
cor(train_variable, train_variable, use = "everything", method = c("pearson", "kendall", "spearman"))

#Correlation for original data set: 
cor(iris[, -5], iris[, -5],use = "everything", method = c("pearson", "kendall", "spearman") )
