#boosting wiht the caret package
#algo available - gbm, mboost, ada, gamBoost: look them up

library(ISLR)
data(Wage)
library(ggplot2)
library(caret)

Wage <- subset(Wage, select=-c(logwage))
inTrain <- createDataPartition(y=Wage$wage, p=.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]

modFIt <- train(wage ~ ., method="gbm", data=training,verbose=FALSE)
print(modFit)


qplot(predict(modFit, testing), wage,data=testing)