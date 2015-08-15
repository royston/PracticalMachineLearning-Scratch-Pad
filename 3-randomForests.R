library(caret)
data(iris)
library(ggplot2)

inTrain <- createDataPartition(y=iris$Species, p=.7, list=FALSE)

training <- iris[inTrain,]
testing <- iris[-inTrain,]

modFit <- train(Species ~ ., data=training, method="rf", prox=TRUE) #rf = random forests, prox gives you extra info that can be used to diagnose

#print the fitted model
print(modFit)


#get a table about the predictions for each split for a given split(in this case 2)
getTree(modFit$finalModel, k=2)

#use the prox above to plot centers for each predicted values
irisP <- classCenter(training[,c(3,4)], training$Species, modFit$finalModel$prox)
irisP <- as.data.frame(irisP);
irisP$Species <- rownames(irisP);
p <- qplot(Petal.Width, Petal.Length, col=Species, data=training)
p + geom_point(aes(x=Petal.Width, y=Petal.Length, col=Species), size=5, shape=4, data=irisP)





