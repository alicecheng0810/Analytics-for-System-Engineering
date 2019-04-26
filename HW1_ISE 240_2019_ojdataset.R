## ISE 240- Spring 2019 from Haiyan Cheng 
# Homework 1. Individual submission.Write your name in the file name, also inside the code.


#Use oj.csv posted in canvas

#weekly sales data of orange juice containers from 83 stores in Chicago area.
# three brands Dominicsk, Tropicana, MinuteMaid

# CREATE AN OBJECT NAMED oj that contains oj data

#Q1) Logmove variable contains the logarithm of sales data. 
#Create a new column namaed "sales" which contains the raw sales values and add "sales" column to oj data

library(readr)
oj <- read_csv("~/Desktop/ISE 240 /HW/HW1/oj.csv")
sales <- exp(oj$logmove)
oj<- cbind(oj,sales)


#Q2) Remove logmove column from oj data set

oj$logmove <- NULL;

#Q3) Remove "sales" object from your environment using rm() 

rm(sales);

#Q4) Create descriptive statistical summary of sales for each brand using tapply() function


tapply(oj$sales, oj$brand, summary);
                          
                            
#Q5) Create histograms fo sales by brand (in a 2by2 screen that can hold 4 plots)

par(mfrow=c(2,2))
hist(oj$sales[oj$brand=="tropicana"], main = "tropicana");
hist(oj$sales[oj$brand=="minute.maid"], main = "minute.maid");
hist(oj$sales[oj$brand== "dominicks"], main = "dominicks");

#Q6)Create a new data frame that holds a subset of oj data with smaller than 40000 sales 

NewData <- subset(oj, oj$sales> 40000)

#Q7) Create histograms of sales in Q5 using the sales information in the new data frame

hist(NewData$sales[oj$week>52 & oj$week < 65], main = "Sales in Q5");

#Q8) Create a single plot that shows "price"  for each brand-side by side. Use lattice library

library(lattice)
xyplot(oj$price~oj$price|oj$brand, main = "Price Comparasion by Brand")

#Continue using the new data frame that holds the subset of oj data
#Q9) a)Does the mean sales of dominicks significantly differ from mean sales of minute.maid? Apply appropriate hypothesis testing method.

     #The sample size is large enough(n>30), so we don't need to perform normality test 
     #Ho: the mean sales of dominicks is not significantly differ from mean sales of minute.maid
     #Ha：the mean sales of dominicks is significantly differ from mean sales of minute.maid

     x = subset(NewData, NewData$brand=="dominicks")
     y = subset(NewData, NewData$brand=="minute.maid")
     var.test(x$sales, y$sales)
     t.test(x$sales,y$sales,alternative="two.sided",var.equal = TRUE)
     #P value is greater than the significance level 0.05. So the mean sales of dominicks is not significantly differ from mean sales of minute.maid



     # b) Does the mean price of dominicks significantly differ from mean price of minute.maid? Apply appropriate hypothesis testing method. 

     #The sample size is large enough(n>30), so we don't need to perform normality test 
     #Ho: the mean price of dominicks is not significantly differ from mean price of minute.maid
     #Ha：the mean price of dominicks significantly differ from mean price of minute.maid
      
     var.test(x$price,y$price)
     t.test(x$price, y$price, alternative="two.sided", var.equal= FALSE)
     #P value is less than the significance level 0.05, So the mean price of dominicks significantly differ from mean price of minute.maid

#Q10) Do prices differ significantly across brands? Use appropriate testing method
     
     aov1<- aov(NewData$price~NewData$brand)
     summary(aov1)
     #F is less than P, so prices differ significantly across brands 


