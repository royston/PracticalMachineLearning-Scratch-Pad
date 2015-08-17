#Q3.
#Load the Alzheimer's disease data using the commands:
#Find all the predictor variables in the training set that begin with IL. 
# Perform principal components on these variables with the preProcess() function from the caret package. 
# the number of principal components needed to capture 80% of the variance. How many are there?
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)

#filter out cols that dont start with "IL"
predictors <- predictors[grep("^IL", names(predictors), value=TRUE)]
print(names(predictors))

adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

#preProcess the data by performing pca after centering and scaling. Note that if you train with method="pca", centering and scaling is done automatically by the caret package
#preProcOutcome <- preProcess(x = subset(adData, select=-c(diagnosis)), thresh = 0.8, method = c("pca","center", "scale"))
preProcOutcome <- preProcess(training[,-1], thresh = 0.9, method = c("pca","center", "scale"))
print(preProcOutcome)

#Q2
#Load the Alzheimer's disease data using the commands:
#Create a training data set consisting of only the predictors with variable names beginning with IL 
#   and the diagnosis. Build two predictive models, one using the predictors as they are and one 
#   using PCA with principal components explaining 80% of the variance in the predictors. 
#   Use method="glm" in the train function. What is the accuracy of each method in the test set? Which is more accurate?

#reuse the training data from previous question
#threshList = list(thresh=.8)
#preProc <- preProcOptions(threshList)

## For the last model:

trainPC <- predict(preProcOutcome, training[,-1])
testPC <- predict(preProcOutcome, testing[,-1])
modFit <- train(training$diagnosis ~ ., method="glm", data=trainPC)
pp<- predict(modFit, testPC)

#confusionMatrix(diagnosis, predict(modFit, testPC))

print(confusionMatrix(testing$diagnosis, pp))

#model without pca:
modFit <- train(training$diagnosis ~ ., method="glm", data=training)
pp <- predict(modFit, testing)
print(confusionMatrix(testing$diagnosis, pp))