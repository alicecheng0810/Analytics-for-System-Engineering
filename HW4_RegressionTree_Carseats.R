# ISE 240 HW 4
# Due Date April 24

# You need to submit a report as a word or PDF document that has all of your answers and explanations, 
# along with the plots created. You also need to submit your R code.

# In Lecture9_Trees_Lab.R file, a classification tree was applied to Carseats data
# set converting Sales variable into a qualitative response. You will now use Sales variable as is ,
# as a quantitative response, and fit a regression tree.

# 1. Again, divide the data into equal sized training and test sets,use set.seed(2)
set.seed(2)
rm(list=ls()) 
library(tree)
library(ISLR)
library(MASS)
attach(Carseats)
smp_size <- floor(0.5*nrow(Carseats))
train_ind <- sample(seq_len(nrow(Carseats)), size = smp_size)
train <- Carseats[train_ind,]
test <- Carseats [-train_ind,]
detach(Carseats)

# 2. Fit a regression tree to the training data, with “Sales” as the response 
tree.Carseats=tree(Sales~., data = train)  
summary(tree.Carseats)
           #Regression tree:
           #tree(formula = Sales ~ ., data = train)
           #Variables actually used in tree construction:
           #  [1] "ShelveLoc"   "Price"       "Age"         "Income"      "CompPrice"   "Population"  "Advertising"
           #Number of terminal nodes:  17 
           #Residual mean deviance:  2.341 = 428.4 / 183 
           #Distribution of residuals:
           #  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
           #-3.76700 -1.00900 -0.01558  0.00000  0.94900  3.58600 

# and the other variables as predictors. Use the “summary()” function to produce summary statistics about the tree, and describe the results obtained. How many terminal nodes does the tree have?
# 3. Create a plot of the tree, and interpret the results.
plot(tree.Carseats)
text(tree.Carseats, pretty=0, cex=0.8)

# 4. Predict the response on the test data. What is the test MSE?
yhat=predict(tree.Carseats, newdata=test)
Carseats.test=Carseats[-train_ind, "Sales"]
plot(yhat, Carseats.test)
abline(0,1)
mean((yhat-Carseats.test)^2)   #mean squared errors 4.8450
#if we want to get the root mean square errors
sqrt(mean((yhat-Carseats.test)^2)) #2.2011

# 5. Apply the “cv.tree()” function to the training set in order to 
# determine the optimal level of tree complexity (tree size).

cv.Carseats = cv.tree(tree.Carseats)  #cv.tree: cross validation for choosing tree complexity


# 6. Produce a plot with tree size on the x-axis and cross-validated deviance on the y- axis
plot(cv.Carseats$size, cv.Carseats$dev, type = 'b')   

# 7. Which tree size is the best ?
#for this particular example, size = 16 is the best

# 8. Produce a pruned tree corresponding to the optimal tree size obtained using cross-validation.
prune.Carseats=prune.tree(tree.Carseats,best=16) 
plot(prune.Carseats)
text(prune.Carseats, pretty = 0)

# 9. Compare the test error between the pruned and unpruned trees. Comment.
yhat_prune = predict(prune.Carseats, newdata = test)
plot(yhat_prune, Carseats.test)
abline(0,1)
mean((yhat_prune-Carseats.test)^2)   #mean squared errors 4.8940
#if we want to get the root mean square errors
sqrt(mean((yhat_prune-Carseats.test)^2)) #2.2122
#MSE pruned = 4.8940 VS MSE unpruned= 4.8450 
