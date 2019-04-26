# PART 1- Linear Regression & Cross Validation

source('~/Desktop/ISE 240 /HW/HW2/Homework 2 .R')
library(readr)
BlackFriday <- read_csv("Desktop/ISE 240 /HW/HW2/BlackFriday.csv")

# Q1) Use set.seed(8). Divide the data set into training set (80% of the data) and test_set (20%)

set.seed(8)
dim(BlackFriday)
smp_size <- floor(0.8*nrow(BlackFriday))
train_ind <- sample(seq_len(nrow(BlackFriday)), size = smp_size)
train <- BlackFriday[train_ind,]
test <- BlackFriday[-train_ind,]

# Q2) Build a linear regression model to predict Purchase using predictors Gender, Age, Occupation,City_Category, Stay_In_Current_City_Years, Marital_Status, Product_Category_1.
# Do not use other variables and make sure categorical variables are considered as “factor” before you build the model

attach(BlackFriday)
Gender <- factor(c("F", "M"))
Occupation <- factor(c(1:20))
City_Category <- factor(c("A", "B", "C"))
Marital_Status <- factor(c("1", "0"))
Age <- factor(c("0-17", "18-25", "26-35", "36-45", "46-50", "51-55", "55+"))

lm.fit = lm(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1, data= train)
summary(lm.fit)
detach(BlackFriday)

#Q3) Which variables are significant predictors of Purchase?

#Coefficients:
#  Estimate Std. Error  t value Pr(>|t|)    
#(Intercept)                  10259.367     51.755  198.228  < 2e-16 ***
#  GenderM                        517.075     16.852   30.683  < 2e-16 ***
#  Age18-25                       317.010     46.920    6.756 1.42e-11 ***
#  Age26-35                       514.661     45.562   11.296  < 2e-16 ***
#  Age36-45                       602.191     46.831   12.859  < 2e-16 ***
#  Age46-50                       613.962     51.430   11.938  < 2e-16 ***
#  Age51-55                       923.873     52.548   17.581  < 2e-16 ***
#  Age55+                         713.123     57.661   12.368  < 2e-16 ***
#  Occupation                       6.746      1.118    6.034 1.60e-09 ***
#  City_CategoryB                 165.841     17.848    9.292  < 2e-16 ***
#  City_CategoryC                 741.679     19.287   38.455  < 2e-16 ***
#  Stay_In_Current_City_Years1     41.113     23.062    1.783   0.0746 .  
#Stay_In_Current_City_Years2     72.237     25.736    2.807   0.0050 ** 
#  Stay_In_Current_City_Years3     24.743     26.160    0.946   0.3442    
#Stay_In_Current_City_Years4+    48.477     26.796    1.809   0.0704 .  
#Marital_Status                 -74.704     15.561   -4.801 1.58e-06 ***
#  Product_Category_1            -416.366      1.922 -216.677  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 4709 on 430044 degrees of freedom
#Multiple R-squared:  0.1069,	Adjusted R-squared:  0.1069 
#F-statistic:  3217 on 16 and 430044 DF,  p-value: < 2.2e-16


# Conclusion: Gender, Age, City_Category, Product_Category_1 

#Q4) Predict Purchase amount using the model you built separately for training set and test set. Calculate the Square root of Mean Square Errors for training and test set predictions. Test set RMSE (root mean squared error) will be the holdout error value.

predit_train <- predict(lm.fit,data = train)
predict_test <- predict(lm.fit, newdata= test)

attach(BlackFriday)
sqrt(mean((Purchase-predict(lm.fit,data=train))^2))
sqrt(mean((Purchase-predict(lm.fit,newdata=test))^2))
detach(BlackFriday)

#Q5) Using cv.glm function, perform a 5-fold cross validation on Black Friday data set using the same predictors and response indicated above in Question2. Calculate cross validation RMSE and compare to the value you found in Q4 using holdout method. Which method would you prefer?

library(boot)
cv.error.5=rep(0,5)
for (i in 1:5){
  glm.fit2=glm(Purchase~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Product_Category_1,data=BlackFriday)
  cv.error.5[i]=cv.glm(BlackFriday,glm.fit2,K=5)$delta[1]   #K=5 argument for 5-fold cross validation
}

k_fold_error_value=sqrt(mean(cv.error.5))
k_fold_error_value
  
# K_fold_error_value is less than the result we got from Q4, so we prefer 5-fold cross validation. 

#PART 2: Logistic Regression
#Use set.seed(9) and only holdout method for this analysis -80:20 ratio again for train and test sets

set.seed(9)

#Q6) Create a histogram of Product_Category_1

hist(BlackFriday$Product_Category_1, xlab = "Product Category 1", ylab = "Sales Quantity", main = "Histogram of Product Category 1")

#Q7) Create a table that shows the counts(frequency) of each Product_Category_1 using table() function). 
     #How many different categories were observed in this variable in BlackFriday data set?

table1 <- table(BlackFriday$Product_Category_1)
table1

#18 different categories 

#Q8) Using the output from Q7 above, calculate the probability of occurrence of each level in Product_category_1. 

prop.table1<- prop.table(table1)
prop.table1

#Q9) Determine the levels of Product_category_1 which has lower than 0.03 probability of occurrence, and consider
#these levels as “low probability levels”.

low_probability_levels <- prop.table1[prop.table1< 0.03]
low_probability_levels

category_of_low_probability <- names(low_probability_levels)
category_of_low_probability
category_of_low_probability <- as.numeric(category_of_low_probability)
category_of_low_probability

#Q10) Create a new variable that takes the value of 0 if an observation’s Product_category_1 level belongs to the “low probability group” described above in Q9. 
#If the level does not belong to the low probability group, new variable will take the value of 1. Add this newly created binary variable to BlackFriday data set

low_probability_level <- ifelse(BlackFriday$Product_Category_1 %in% category_of_low_probability, "0", "1")
low_probability_level <- as.factor(low_probability_level)

new_data <- cbind(BlackFriday, low_probability_level)

#Q11) Build a logistic regression model that predicts whether a purchase will belong to a low probability level or not. 
#Use predictors Gender,Age,Occupation,City_Category,Stay_In_Current_City_Years,Marital_Status,Purchase

dim(new_data)
smp_size <- floor(0.8*nrow(new_data))
train_ind <- sample(seq_len(nrow(new_data)), size = smp_size)
train_new <- new_data[train_ind,]
test_new <- new_data[-train_ind,]

logistic_model<-glm(low_probability_level~Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status+Purchase,data=train_new,family="binomial")
summary(logistic_model)

#Q12) Calculate Test set misclassification rate and accuracy of your model using different threshold(cutoff) probabilities. 
#Which cutoff value would you choose? Why?

predict_logistic_model <- predict(logistic_model, newdata = test_new, type = "response")
predict_logistic_model_set <- ifelse(predict_logistic_model > 0.90, 1, 0)
predict_logistic_model

misclassification <- table(test_new$low_probability_level, predict_logistic_model_set)
misclassification

#Cutoff = 0.90, misclassification = 0.371, accuracy = 0.629
(6152+33739)/(4097+6152+33739+63528) 
1-0.371

#Cutoff = 0.89, misclassification = 0.213, accuracy = 0.787
(8332+14523)/(1917+8332+14523+82744)
1-0.213

#Cutoff = 0.88, misclassification = 0.11, accuracy = 0.89
(9927+1900)/(322+9927+1900+95367)
1-0.11

#Cutoff = 0.87, misclassification = 0.095, accuracy= 0.905
(10240+12)/(9+10240+12+97255)
1-0.095 

install.packages("InformationValue")
library(InformationValue)
optCutOff <- optimalCutoff(test_new$low_probability_level, predict_logistic_model)[1] 
optCutOff
#optCutOff = 0.871, so choose 0.87  

