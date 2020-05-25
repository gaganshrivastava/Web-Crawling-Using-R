##-------------------------------------------------------------
# machine learning Data preprocessing

setwd("G:\\NJIT\\Tutorials Notes\\R\\Books\\original_ML_datasets")

mydata = read.csv(file="G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 1 - Data Preprocessing/Section 2 -------------------- Part 1 - Data Preprocessing --------------------/R/Data.csv")

a =mydata

# on what basic we have to predict the dependent variable
a = a[,2:3]

# one way to clean the data is to remove the NA rows or coloumns
#we can do that by complete.cases(), or is.na
a = a[complete.cases(a),]
# OR
a = a[!is.na(a$Salary),]

rm(a)

# replace the missing value with mean value
# taking the age cooumns
a =mydata

is.na(a$Age)

#ave is the inbuilt function for the average group value
a$Age = ifelse((is.na(a$Age)), 
               ave(a$Age,  FUN = function(x) mean(x,na.rm = T)),
                a$Age)

a$Salary = ifelse((is.na(a$Salary)), 
                  ave(a$Salary, FUN = function(x) mean(x, na.rm = T)),
                  a$Salary)

# replaceing with meadin vaalue , just replace the mean with median
# a$Salary = ifelse((is.na(a$Salary)), 
#                   ave(a$Salary, FUN = function(x) median(x, na.rm = T)),
#                   a$Salary)


#dealing with categorical data
# a has 2 categorical data, i.e country and purchased
# all the data in ML models must be n the number form 
#so converting the char to number or factor

a
str(a)
levels(a$Country)
a$Country = factor(a$Country, 
                   levels = c("France" , "Germany", "Spain"),  #levels gives the difernet values of country
                   labels = c(1,2,3))    # labels can set the values on the basic of levels

levels(a$Purchased)
a$Purchased = factor(a$Purchased, 
                   levels = c("No" , "Yes"),  #levels gives the difernet values of country
                   labels = c(0,1))


##spliting the data into train andd test data set
# catools is used to spliting the data
library(caTools)

set.seed(123)
#sample.split will take the dependent variable and the percent of ttrain and test means, 
# 80 percent is the training and 20 percent is the test  data
splitt  = sample.split(a$Purchased, SplitRatio = .8)
splitt
trainning = subset(a,splitt == T)
trainning

testting = subset(a,splitt == F)
testting


## check for the outliers , also called feature scaling
boxplot(a$Salary)
boxplot(a$Age)

plot(a$Age,a$Salary)

# as you can see that salary variable is in thounsdands while ageis very less
#so salary is dominating , so we have to make these two variable on the same scale

#  2 methods of fetaure scaling
# standarization and nomalization
# x - mean(x)/sd(x)   and x - min(x)/diff(range(x))


# scaling of variables
# scale will make the varialbe in the range of -1 to 1

trainning[,2:3] = scale(trainning[,2:3])

testting[,2:3] = scale(testting[,2:3])


#####################################################
## we are done with the data preprocessing, make the data ready to do the model


#----------------------Regression--------------------------------------
# simple linear regresssion

salarydata= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/Section 4 - Simple Linear Regression/R/Salary_Data.csv")

plot(salarydata$YearsExperience,salarydata$Salary)

# so step 1 , find the independent and dependent variable
#so, salary is the dependent variable , depends on experiesnce

# check the missing values, F menas no missing values
any(is.na(salarydata))

# no need of scaling in this as it is just one independent variable

# spliting the data set
splitt = sample.split(salarydata$Salary, SplitRatio = 2/3)

splitt
trainning  = subset(salarydata, splitt == T)
testting = subset(salarydata, splitt == F)

# simple liner progression model take care of scaling


## making the model with lm()
?lm()

regressor = lm(formula = trainning$Salary~ trainning$YearsExperience,  #dependent variable~ independent variable
               data = trainning )  # data on which we have to make the model
    
regressor
    
summary(regressor)

##Coefficients: is very useful  in this
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# data is perfect(significant)  , less signitficant>>>>>>*** = best data, no star means bad data

#more lower the p value is tels the more significant the data is
# or if p value is below 5% then indepent variable is higly significant and vice versa
#p-value: < 2.2e-16

# Coefficients:
#                             Estimate Std. Error  t value  Pr(>|t|)    
# (Intercept)                 25792.2     2273.1   11.35 5.51e-12 ***
#   salarydata$YearsExperience   9450.0      378.8   24.95  < 2e-16 ***


##predict the test set result based on model prepare

y_predict = predict(regressor,   ## give the model
                    newdata = testting)  ## give the data set of testing

y_predict

##visulizing the training set and testing test
library(ggplot2)
attach(salarydata)

# start with the poing, then line, then axis
nrow(trainning)

ggplot() +
  geom_point(aes(x = trainning$YearsExperience, y = trainning$Salary ),
             colour = 'red') +
  geom_line(aes(x = trainning$YearsExperience, y = predict(regressor, newdata = trainning)),
            colour = 'blue') +
  ggtitle('Salaray vs Expereince of Traning Set') +
  xlab('Year of experience')+
  ylab('Salary')

## plot with the test data,, in the line we have to mke with the traning set
ggplot() +
  geom_point(aes(x =  testting$YearsExperience, y = testting$Salary ),
             colour = 'red') +
  geom_line(aes(x = trainning$YearsExperience, y = predict(regressor, newdata = trainning)),
            colour = 'blue') +
  ggtitle('Salaray vs Expereince of Traning Set') +
  xlab('Year of experience')+
  ylab('Salary')



##---------------------------------------------------------------------
## -----------------Multiple Linear regression------------------

# it has one dependent varialbe and many independent variable

startupdata = read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/Section 5 - Multiple Linear Regression/R/50_Startups.csv")

startupdata
str(startupdata)

unique(startupdata$State)

startupdata$State = factor(startupdata$State,
                           levels = c("New York","California","Florida"),
                           labels = c(1,2,3))

set.seed(123)

splitt= sample.split(startupdata$Profit,  SplitRatio = .8)

traning = subset(startupdata, splitt == T)
test = subset(startupdata, splitt == F)

#scaling will be handle by model only

##making the multiple liner regression model

regressssor = lm(formula = traning$Profit ~ ., data =  traning)

summary(regressssor)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      4.965e+04  7.637e+03   6.501 1.94e-07 ***
#   R.D.Spend        7.986e-01  5.604e-02  14.251 6.70e-16 ***
#   Administration  -2.942e-02  5.828e-02  -0.505    0.617    
# Marketing.Spend  3.268e-02  2.127e-02   1.537    0.134    
# State2           1.213e+02  3.751e+03   0.032    0.974    
# State3           2.376e+02  4.127e+03   0.058    0.954  

## with the help of summary coffiecnt we can see the only "R.D.Spend " is the strong predictor

# so chnaging the model formula
regressssor = lm(formula = Profit ~ R.D.Spend, data =  traning)

summary(regressssor)

y_predict = predict(regressssor, newdata = test)

y_predict


#automatic implementation of Backward Elimination in R, here it is:

backwardElimination <- function(x, sl) {
  numVars = length(x)
  for (i in c(1:numVars)){
    regressor = lm(formula = Profit ~ ., data = x)
    maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
    if (maxVar > sl){
      j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
      x = x[, -j]
    }
    numVars = numVars - 1
  }
  return(summary(regressor))
}

SL = 0.05
dataset = startupdata[, c(1,2,3,4,5)]
backwardElimination(dataset, SL)



##-----------------------------------------------------------------------
#3 --------------------------------------Ploynomial regresiion model---------


data_set = read.csv(file= "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/Section 6 - Polynomial Regression/R/Position_Salaries.csv")

data_set = data_set[2:3]
str(data_set)

plot(data_set$Level,data_set$Salary)
# fromm the plot we can see that its a non liner model

# as the data is small so no need to split

# making the lenaner regression model
lm_reg = lm(formula =Salary~Level, data = data_set)

summary(lm_reg)
predict_lm_my = predict(lm_reg, newdata = data.frame(Level = 6.5))

## making the polynomial regression model
data_set$Level2 = data_set$Level^2
data_set$Level3 = data_set$Level^3
data_set$Level4 = data_set$Level^4

ploy_reg = lm(formula = Salary~., data = data_set)

summary(ploy_reg)

pploy_predict = predict(ploy_reg, newdata = data_set)


# making our own prediction
pploy_predict_my = predict(ploy_reg, newdata = data.frame(Level = 6.5,
                                                          Level2 = 6.5^2,
                                                          Level3 = 6.5^3,
                                                          Level4= 6.5 ^4))

pploy_predict_my
## visulazing the liner regression

ggplot()+
  geom_point(aes(x = data_set$Level, y = data_set$Salary),
             colour = 'red') +
  geom_line(aes(x= data_set$Level, y =predict(lm_reg, newdata = data_set)),
            colour = 'blue')


## vsulizing polynomial plot

ggplot()+
  geom_point(aes(x = data_set$Level, y = data_set$Salary),
             colour = 'red') +
  geom_line(aes(x= data_set$Level, y =pploy_predict),
            colour = 'blue')


##-------------------------------------------------------------------
#Support vector Regression----------------------------------------
#SVR is a powerful algorithm that allows us to choose how tolerant we are of errors, both through an acceptable error margin(??) and through tuning our tolerance of falling outside that acceptable error rate.
# it is insie SVM

data_set = read.csv(file ="G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/Section 7 - Support Vector Regression (SVR)/R/Position_Salaries.csv")

library(e1071)
# type = is for the classsification or regression model..
regressor = svm(formula = Salary~., data = data_set, 
                type = 'eps-regression')

summary(regressor)


y_predict = predict(regressor, newdata = data_set)

y_predict



y_predict = predict(regressor, data.frame(Level = 6.5))

#visulize the polt
ggplot()+
  geom_point(aes(x = data_set$Level, y = data_set$Salary),
             colour = 'red') +
  geom_line(aes(x= data_set$Level, y =y_predict),
            colour = 'blue')

# SVR is not that good for outlier


#####-------------------------------------------------------------------------
##Decision Tree Regression ----------------------------------------


data_set = read.csv(file ="G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/Section 7 - Support Vector Regression (SVR)/R/Position_Salaries.csv")

library(rpart)
?rpart

regressor = rpart(formula = Salary ~ .,
                  data = dataset,
                  control = rpart.control(minsplit = 1))   # control is used give thee spilt

x_grid = seq(min(data_set$Level), max(data_set$Level), 0.01)
ggplot() +
  geom_point(aes(x = data_set$Level, y = data_set$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')



#####-------------------------------------------------------------------------
##Random forest  Regression ----------------------------------------


data_set = read.csv(file ="G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 2 - Regression/Section 7 - Support Vector Regression (SVR)/R/Position_Salaries.csv")

library(randomForest)
?randomForest

regressor = randomForest(x = data_set[2],
                         y = data_set$Salary,
                         ntree = 200)

y_predict = predict(regressor, newdata = data_set)

y_predict
# x grid is for better resolution
x_grid = seq(min(data_set$Level), max(data_set$Level), 0.01)
ggplot() +
  geom_point(aes(x = data_set$Level, y = data_set$Salary),
             colour = 'red') +
  geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
            colour = 'blue') +
  ggtitle('Truth or Bluff (Decision Tree Regression)') +
  xlab('Level') +
  ylab('Salary')





#-----------------------------------------------------------------------
#-----------------------------------------------------------------------
#                Classification Learning
#-----------------------------------------------------------------------
#-----------------------------------------------------------------------

# Unlike regression where you predict a continuous number, 
# you use classification to predict a category.
# There is a wide variety of classification applications from medicine to marketing. 
# Classification models include linear models like Logistic Regression, SVM,
# and nonlinear ones like K-NN, Kernel SVM and Random Forests.


#------------------------------------------------------------------------------------
#------------------------Logistic Regression


dataset= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 3 - Classification/Section 14 - Logistic Regression/R/Social_Network_Ads.csv")

dataset = dataset[3:5]
unique(dataset$Purchased)

str(dataset)
dataset$Purchased = factor(dataset$Purchased,
                           levels = c(0,1),
                           labels = c(0,1))


library(caTools)
set.seed(123)

dataset[,2:3] = scale(dataset[,2:3])

split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


## classification model need to do the scaling 

# Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[-3] = scale(test_set[-3])

colnames(dataset)
## making the model
?glm
?cv.glmnet
# install.packages("glmnet", repos = "https://cran.us.r-project.org")
library(glmnet)
?glmnet
?cv.glmnet
# Given feature matrix X_train and label vector y_train, please write an R command to perform
# Logistic Regression using 5-fold cross validation with the loss of AUC.
# cv.glmnet(x = x_train, y= y_train,  nfolds = 5,type.measure = 'auc')
# for logistic we have to put famil as binomial



?glm
classifer = glm(formula = Purchased~. , family = binomial,
                data = training_set)

# predcition 
#-3 boc, we are predicting last cloulm, type has to be response

prob_pred = predict(classifer, newdata = test_set[-3], type = 'response')

prob_pred

## to get  the prob in 0 & 1
prob_pred = ifelse(prob_pred > .5, 1,0)

summary(classifer)

#making the confusion matrix
cm = table(test_set$Purchased,prob_pred)
cm


##Visulization Part
# Visualising the Training set results

install.packages('ElemStatLearn')
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))



#--------------------------------------------------------------------
#KNN Classification



dataset= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 3 - Classification/Section 15 - K-Nearest Neighbors (K-NN)/R/Social_Network_Ads.csv")

dataset = dataset[3:5]
unique(dataset$Purchased)

str(dataset)
dataset$Purchased = factor(dataset$Purchased,
                           levels = c(0,1),
                           labels = c(0,1))


library(caTools)
set.seed(123)



split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


## classification model need to do the scaling 

# Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[-3] = scale(test_set[-3])

##-- preprocessing done
##making KNN -model  and predicting test set results
library(class)

?knn
#we are trining on first  coloumns, and testing on thse colooumns only
#cl is for categorial dependent variable
#k is no of nebours
y_pred = knn(train = training_set[,-3],
             test = test_set[,-3],
             cl = training_set[,3],
             k = 5
             )

y_pred

## accuarcy ::
table(test_set$Purchased,y_pred)

## we can make the plot like above



####--------------------------------------------------------------
## SVM 


dataset= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 3 - Classification/Section 15 - K-Nearest Neighbors (K-NN)/R/Social_Network_Ads.csv")

dataset = dataset[3:5]

str(dataset)
dataset$Purchased = factor(dataset$Purchased,
                           levels = c(0,1),
                           labels = c(0,1))


library(caTools)
set.seed(123)

split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


## classification model need to do the scaling 

# Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[-3] = scale(test_set[-3])


## making the models

library(e1071)
?svm
classifier = svm(formula = Purchased~.,
                 data = training_set,
                 type = 'C-classification',
                 kernel= 'linear')  # linear is simple kernel type, it means our svm classifier is a linear classifier,  when we draw the plot it will be linear


y_predict= predict(classifier, test_set[-3])

y_predict

table(test_set[,3],y_predict)



#----------------------------------------------------------
#Kernel SVM  classisiffication
# SVM is used when we can find a boundry between two classes.... 
# now if we can 't  find a boundry between two claesses then use the kernel svm
# mens  data is not linearly sepearable'

# to make the data points seperable we increase the dimension.. higer diemension data can split the data points and we can make the line between them
?svm


dataset= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 3 - Classification/Section 15 - K-Nearest Neighbors (K-NN)/R/Social_Network_Ads.csv")

dataset = dataset[3:5]

str(dataset)
dataset$Purchased = factor(dataset$Purchased,
                           levels = c(0,1),
                           labels = c(0,1))


library(caTools)
set.seed(123)

split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


## classification model need to do the scaling 

# Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[-3] = scale(test_set[-3])

##making the kernal svm model

library(e1071)
#kernel = gaussian = radial
clasifer = svm(formula = Purchased~ .,
               data = training_set,
               type = "C-classification",
               kernel = 'radial')

y_pred = predict(clasifer, test_set)

table(test_set$Purchased, y_pred)


#---------------------------------------------------------------------
##Bayes  theorem

#  nayve bayes classsifier

library(e1071)
dataset= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 3 - Classification/Section 15 - K-Nearest Neighbors (K-NN)/R/Social_Network_Ads.csv")


dataset = dataset[3:5]

# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

##making  navie model

# install.packages('e1071')
library(e1071)
classifier = naiveBayes(x = training_set[-3],
                        y = training_set$Purchased)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-3])

# Making the Confusion Matrix
cm = table(test_set[, 3], y_pred)
cm


#_--------------------------------------------------------------
## Decision tree classsisfication

dataset= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 3 - Classification/Section 15 - K-Nearest Neighbors (K-NN)/R/Social_Network_Ads.csv")


dataset = dataset[3:5]

str(dataset)
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# it is not that necesaaary for decision tress, but using it can give the good results (better resolution)
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

## making the model
library(rpart)

colnames(training_set)

classifier = rpart(formula = Purchased ~ .,
                   data = training_set)

y_pred = predict(classifier, newdata =test_set[-3] )

#Or to gett he prediction value in a factor()

y_pred = predict(classifier, newdata =test_set[-3], type = 'class' )

table(test_set[,3],y_pred)

plot(classifier)
text(classifier)


#------------------------------------------------------------------------------------------------------
#Random forest classification


dataset= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 3 - Classification/Section 15 - K-Nearest Neighbors (K-NN)/R/Social_Network_Ads.csv")


dataset = dataset[3:5]

str(dataset)
# Encoding the target feature as factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0, 1))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# it is not that necesaaary for decision tress, but using it can give the good results (better resolution)
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])

str(training_set)
## making the model
library(randomForest)
?randomForest
classifier = randomForest(x = training_set[-3],
                          y = training_set$Purchased,
                          ntree = 10)

y_pred = predict(classifier, test_set[-3])

table(test_set[,3],y_pred)


# ##--------------------------------------------------------------------------
# 
# If your problem is linear, you should go for Logistic Regression or SVM.
# 
# If your problem is non linear, you should go for K-NN, Naive Bayes, Decision Tree or Random Forest.
# 
# # - Logistic Regression or Naive Bayes when you want to rank your predictions by their probability. For example if you want to rank your customers from the highest probability that they buy a certain product, to the lowest probability. Eventually that allows you to target your marketing campaigns. And of course for this type of business problem, you should use Logistic Regression if your problem is linear, and Naive Bayes if your problem is non linear.
# # 
# # - SVM when you want to predict to which segment your customers belong to. Segments can be any kind of segments, for example some market segments you identified earlier with clustering.
# # 
# # - Decision Tree when you want to have clear interpretation of your model results,
# # 
# # - Random Forest when you are just looking for high performance with less need for interpretation. 


#-------------------------------------------------------------------------------
##Unsupervised learning-----------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# k means clustering


dataset = read.csv(file= "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 4 - Clustering/Section 24 - K-Means Clustering/R/Mall_Customers.csv")

dataset = dataset[4:5]


##using the elbow method to find the optimal no of cluster
## to get the same no. use seed
?kmeans
set.seed(6)
wcss = vector()
for(i in 1:10){
  wcss[i] = sum(kmeans(dataset,i)$withinss)
}

?plot
#type = 'b' is for both line and point#
plot(1:10,wcss,type = 'b', xlab = "cluster",ylab = "wwcsss")

## so from the plot we can see that 5 is the best no of cluster to o..

#So k = 5

## making model
model_k = kmeans(dataset,5,iter.max = 300,nstart = 10)

y_kmeans = model_k$cluster

# Visualising the clusters
library(cluster)

?clusplot
clusplot(dataset,       ## data set
         y_kmeans,      ## which cluster belong to
         lines = 0,     ##so that no distance line willl aperar in plot
         shade = TRUE,##
         color = TRUE,
         labels = 2,# all the labels
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


####--------------------------------------------------------

#Hirerical clustering

dataset = read.csv(file= "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 4 - Clustering/Section 24 - K-Means Clustering/R/Mall_Customers.csv")

dataset = dataset[4:5]

# Using the dendrogram to find the optimal number of clusters
?hclust
 dendrogram = hclust(d = dist(dataset, method = 'euclidean'),
                     method = 'ward.D')
plot(dendrogram,
     main = paste('Dendrogram'),
     xlab = 'Customers',
     ylab = 'Euclidean distances')

# Fitting Hierarchical Clustering to the dataset
hc = hclust(d = dist(dataset, method = 'euclidean'), method = 'ward.D')
?cutree
y_hc = cutree(hc, 5)

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels= 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------
## model performace check
#method 1
# k folds cross validation

#part-10
## using the kernel svm model
# papprameters thta we choose ourself in the model is called hyper parameters


dataset= read.csv(file = "G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 3 - Classification/Section 15 - K-Nearest Neighbors (K-NN)/R/Social_Network_Ads.csv")

dataset = dataset[3:5]

str(dataset)
dataset$Purchased = factor(dataset$Purchased,
                           levels = c(0,1),
                           labels = c(0,1))


library(caTools)
set.seed(123)

split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


## classification model need to do the scaling 

# Feature Scaling
training_set[,1:2] = scale(training_set[,1:2])
test_set[-3] = scale(test_set[-3])

##making the kernal svm model
#---------------------------------

library(e1071)
#kernel = gaussian = radial
clasifer = svm(formula = Purchased~ .,
               data = training_set,
               type = "C-classification",
               kernel = 'radial')

y_pred = predict(clasifer, test_set)


cm = table(test_set$Purchased, y_pred)

cm

acc = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
acc

#-----------------------------------------------------------------
## appliying k folds cross validation
library(caret)
?createFolds
folds = createFolds(training_set$Purchased,k = 10)
folds

training_set[folds[[1]],]


cv = lapply(folds, function(x){
  train_folds = training_set[-x,]
  test_folds = training_set[x,]
  clasifer = svm(formula = Purchased~ .,
                 data = train_folds,
                 type = "C-classification",
                 kernel = 'radial')
  y_pred = predict(clasifer, test_folds)
  
  cm = table(test_folds$Purchased, y_pred)
  acc = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  return(acc)
  
})

cv
# taking the means of all 10 acc we got to get the optimal accuaaracy
accuracy = mean(as.numeric(cv))
accuracy


##--------------------------------------------------------------
#Method 2--
#grid search of model performance improving
# we can improve the model performace wtih the help of good hyper parameters

library(caret)
# train function is inside the caret library, by which we can make the model directly by give the model name in the method parameter
## very imp link to see the different method name of train function for different models
#http://topepo.github.io/caret/index.html
#http://topepo.github.io/caret/available-models.html
# we can make the models by both caret , train function or by direct model name of specific model
# for parameter tuing we use the caret pacakage

#for svm kernel = svmRadial = method name
?train()

gredclassifier = train(form = Purchased~.,
                       data = training_set,
                       method = 'svmRadial')

gredclassifier

gredclassifier$bestTune

?svm

library(e1071)
#kernel = gaussian = radial
## checking by giving the addition paramter
clasifer = svm(formula = Purchased~ .,
               data = training_set,
               type = "C-classification",
               kernel = 'radial',
               sigma = 1.433809,
               C = 1)

y_pred = predict(clasifer, test_set)


cm = table(test_set$Purchased, y_pred)

cm

acc = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
acc


#---------------------------------------------------------------------------
## Associated rule learning
##apriori algo



setwd("G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 5 - Association Rule Learning/Section 28 - Apriori/R")


# Apriori

# Data Preprocessing
# install.packages('arules')
library(arules)
dataset = read.csv('Market_Basket_Optimisation.csv', header = FALSE)
dataset = read.transactions('Market_Basket_Optimisation.csv', sep = ',', rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

# Training Apriori on the dataset
rules = apriori(data = dataset, parameter = list(support = 0.004, confidence = 0.2))

# Visualising the results
inspect(sort(rules, by = 'lift')[1:10])


##---------------------------------------------------------------------------------------------
## Reinforcemnt learning

Dataset = read.csv(file ="G:/NJIT/Tutorials Notes/R/Books/original_ML_datasets/Machine Learning A-Z (Codes and Datasets)/Part 6 - Reinforcement Learning/Section 32 - Upper Confidence Bound (UCB)/R/Ads_CTR_Optimisation.csv")
