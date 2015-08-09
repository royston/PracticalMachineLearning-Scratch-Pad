#Need to condense multiple variables together in some cases(in cases when they are highly correlated)
library(caret)
library(kernlab)
data(spam)

inTrain <- createDataPartition(y=spam$type, p=.75, list=FALSE)
training <- spam[inTrain,]
testing <- spam[-inTrain,]


M <- abs(cor(training[,-58]))

#every variable has a correlation with itself. ignore these
diag(M) <- 0
which(M > 0.8, arr.ind=T) # indicates that num415 and num857 have a high correlation

plot(x = training$num415, y = training$num857)

#one way to condense these 2 is to rotate the plot - read up about this and other techniques of PCA

# R allows you to do PCA with the prcmp function
#      allows you to correlate more than 2 variables
smallSpam <- spam[,c(34, 32)]
prComp <- prcomp(smallSpam) # this object internally has a rotation matrix as above
plot(prComp$x[,1],prComp$x[,2])

#eg for more than 2 variables:
typeColor <- ((spam$type == "spam")*1 + 1)
prComp <- prcomp(log10(spam[,-58]+1))
plot(prComp$x[,1],prComp$x[,2], col=typeColor)

#possible to do pass in "pca" as an argument to the train and predict functions to have pca done along with the training/prediction process
