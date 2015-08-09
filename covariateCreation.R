library(ISLR)
data(Wage)
library(caret)

inTrain <- createDataPartition(y=Wage$wage, p=0.7, list=FALSE)

training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

#covariates are variables that you will actually include in the model
# need to convert raw data into variables that you can fit standard ML algos to
# need to summarize the data as well as possible, while making sure there is no information loss


#One technique is to convert qualitative variables into dummy variables
# eg Job class
table(training$jobclass)

#convert using the dummyVArs funtion
dummies <- dummyVars(wage ~ jobclass, data=training) # gives you an object
head(predict(dummies, newdata=training))


#2. Removing zero covariates - variables that have very low variability. 
#   eg, all subjects are MALE
nsv <- nearZeroVar(x = training, saveMetrics = TRUE) # shows you a table with variables and metrics that measure variability
