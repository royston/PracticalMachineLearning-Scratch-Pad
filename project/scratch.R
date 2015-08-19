training <- read.csv("T:/dev/ws/DataScience/PracticalMachineLearning/PredictingWithTrees/PracticalMachineLearning/PracticalMachineLearning-Scratch-Pad/project/pml-training.csv")
testing <- read.csv("T:/dev/ws/DataScience/PracticalMachineLearning/PredictingWithTrees/PracticalMachineLearning/PracticalMachineLearning-Scratch-Pad/project/pml-testing.csv")

library(caret)


#pre process the data. take out variables that have near zero variance. the outcome is clearly independent of them
nsv <- nearZeroVar(x=training)

#nsv contains the column numbers where we have near zero variance
# reference : http://topepo.github.io/caret/preprocess.html#nzv
training <- training[-nsv]

#extract the numeric cols
numericCols <- training[sapply(training, is.numeric)]
corrMatrix <- cor(numericCols, use = "na.or.complete")
highCorrMatrix <- findCorrelation(x = corrMatrix, cutoff = .75)

training <- training[,-highCorrMatrix]
summary(training)


#remove cols that are duplicated - verify that cvtd_timestamp is duplicate

#notice a bunch of cols that have NA. we need to impute the data based on similar rows. the k nearest neighbors algo does this for us and is part of the caret package
preProcObj <- preProcess(x=numericCols, method=c("knnImpute", "pca"))

modFit <- train(classe ~ ., x = training, preProcess = preProcObj, method = "rf")

