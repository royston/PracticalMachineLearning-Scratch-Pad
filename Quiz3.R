library(AppliedPredictiveModeling)
library(caret)
library(ElemStatLearn)
library(pgmm)
library(rpart)


# q1
# Question 1
# Load the cell segmentation data from the AppliedPredictiveModeling package using the commands:
#   library(AppliedPredictiveModeling)
# data(segmentationOriginal)
# library(caret)
# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
# 3. In the final model what would be the final model prediction for cases with the following variable values:
#   a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 

data(segmentationOriginal)
set.seed(125)
inTrain <- createDataPartition(segmentationOriginal$Case, p=0.6)[[1]]
training <- segmentationOriginal[inTrain,]
testing <- segmentationOriginal[-inTrain,] 


modFit <- train(Class ~ ., method="rpart", data = training)
print(modFit$finalModel)


#notice that the question omits most of the variables, turns out it is sufficient to examing the tree that is constructed for this model and manually determin the outcomes for specific test cases
print(head(testing))
plot(modFit$finalModel)
text(modFit$finalModel)

# 
# Question 3
# Load the olive oil data using the commands:
#   
#   library(pgmm)
# data(olive)
# olive = olive[,-1]
# (NOTE: If you have trouble installing the pgmm package, you can download the olive dataset here: olive_data.zip. After unzipping the archive, you can load the file using the load() function in R.)
# These data contain information on 572 different Italian olive oils from multiple regions in Italy. Fit a classification tree where Area is the outcome variable. Then predict the value of area for the following data frame using the tree command with all defaults
install.packages('pgmm')
install.packages('prodlim')
library(pgmm)
library(caret)
data(olive)
olive = olive[,-1]
inTrain <- createDataPartition(olive$Area, p=1.0)[[1]]
training <- olive[inTrain,]
testing <- olive[-inTrain,] 
modFit <- train(Area ~ ., method="rpart", data = training)

#manually examine the tree
plot(modFit$finalModel)
text(modFit$finalModel)
predict(modFit, newdata = as.data.frame(t(colMeans(olive))))



# Load the South Africa Heart Disease Data and create training and test sets with the following code:
#   library(ElemStatLearn)
# data(SAheart)
# set.seed(8484)
# train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
# trainSA = SAheart[train,]
# testSA = SAheart[-train,]
# 
# Then set the seed to 13234 and fit a logistic regression model (method="glm", be sure to specify family="binomial") with Coronary Heart Disease (chd) as the outcome and age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, and low density lipoprotein cholesterol as predictors. Calculate the misclassification rate for your model using this function and a prediction on the "response" scale:
#   
#   missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
# What is the misclassification rate on the training set? What is the misclassification rate on the test set?
install.packages('ElemStatLearn')
library(ElemStatLearn)
library(caret)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
set.seed(13234)
SAheart <- SAheart[c('age', 'alcohol', 'obesity', 'tobacco', 'typea', 'ldl', 'chd')]

trainSA <- trainSA[c('age', 'alcohol', 'obesity', 'tobacco', 'typea', 'ldl', 'chd')]
testSA <- testSA[c('age', 'alcohol', 'obesity', 'tobacco', 'typea', 'ldl', 'chd')]

modFit <- train(chd ~ ., method="glm", data = trainSA, family = "binomial")

trainPrediction <- predict(modFit, newdata = trainSA)
testPrediction <- predict(modFit, newdata = testSA)

missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
missClass(trainSA$chd, trainPrediction)
missClass(testSA$chd, testPrediction)
# 
# Load the vowel.train and vowel.test data sets:
#   library(ElemStatLearn)
# data(vowel.train)
# data(vowel.test) 
# Set the variable y to be a factor variable in both the training and test set. Then set the seed to 33833. Fit a random forest predictor relating the factor variable y to the remaining variables. Read about variable importance in random forests here: http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr The caret package uses by defualt the Gini importance. Calculate the variable importance using the varImp function in the caret package. What is the order of variable importance?
library(ElemStatLearn)
library(caret)
data(vowel.train)
data(vowel.test) 
set.seed(33833)

#need to say importance=TRUE for it to parallelly also calculate imporance of variables
modFit <- train(y ~ ., method="rf", data = vowel.train, importance=TRUE)

varImp(modFit$finalModel)