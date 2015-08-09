#predicting with regression
# idea is to fit a line to a set of data; get new predictors/covariates; multiple them by coefficients

#using data on eruption of gysers
library(caret); data(faithful); set.seed(333)

inTrain <- createDataPartition(y=faithful$waiting, p=.5, list=FALSE)

training <- faithful[inTrain,]
testing <- faithful[-inTrain,]

plot(training$waiting, training$eruptions, pch=19, col="blue", xlab="Waiting", ylab="Duration")

#above data plot shows you can fit a linear model
lm1 <- lm(eruptions ~waiting, data=training)
summary(lm1)

#lm1 contains parameters and coefficients for the linear model
plot(training$waiting, training$eruptions, pch=19, col="blue")
lines(training$waiting, lm1$fitted, lwd=3) # fit a line through the data with the linear model with just trained
#the coef command with give you the paramets of the equation
coef(lm1)[1] + coef(lm1)[2]*80

#predict a new value
newdata <- data.frame(waiting=80) #create a dataframe with a single row with waiting time of 80

#use the above trained linear model to predict on this 
predict(lm1, newdata)
