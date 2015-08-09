#predicting wages 

library(ISLR)
library(ggplot2); library(caret);

data(Wage);
Wage <- subset(Wage, select=-c(logwage))

summary(Wage)
#we see that data only has males

inTrain <- createDataPartition(y=Wage$wage, p=.7, list=FALSE)
training <- Wage[inTrain,]
testing <- Wage[-inTrain,]


#explore that data. plot a featureplot
featurePlot(x=training[,c("age","education", "jobclass")], y=training$wage, plot="pairs")
#more plotting
qplot(age, wage, colour=education, data=training)

#fit a linear model
modFit <- train(wage ~ age + jobclass + education, method="lm", data=training)
finMod <- modFit$finalModel
print(modFit) # notice we have 10 predictors even though we only specified 4 variables - this is because education and jobclass have been expanded

#diagnostics on the model
plot(finMod, 1, pch=19, cex=.5, col="green") #plots predictions from model vs the difference between actual and real(should be close to zero). Notice there a few outliers
qplot(finMod$fitted, finMod$residuals,colour=race,data=training) #same as above, but colour the data points based on race. The outliers seem to be because of reace
plot(finMod$residuals, pch=19) #shows a bunch of outlier for higher indexes - indicates that it may be because the data is dependent on time series

#plot predicted vs test set 
pred <- predict(modFit, testing)
qplot(wage, pred, colour=year, data=testing)











