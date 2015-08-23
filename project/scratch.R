data <- read.csv("T:/dev/ws/DataScience/PracticalMachineLearning/PredictingWithTrees/PracticalMachineLearning/PracticalMachineLearning-Scratch-Pad/project/pml-training.csv", na.strings = c("NA", ""))

library(caret)
library(randomForest)


inTrain <- createDataPartition(y = data$classe, list = FALSE, p = .7)
training <- data[inTrain,]
testing <- data[-inTrain,]


#pre process the data. take out variables that have near zero variance. the outcome is clearly independent of them
nsv <- nearZeroVar(x=training)

#nsv contains the column numbers where we have near zero variance
# reference : http://topepo.github.io/caret/preprocess.html#nzv
training <- training[-nsv]
naCutOff = dim(training)[1] * 0.8
training <- training[-which(as.numeric((colSums(is.na(training)))) > naCutOff)]

#remove index, name, timestamps
training <- training[,7:dim(training)[2]]

#extract the numeric cols
numericCols <- training[sapply(training, is.numeric)]
corrMatrix <- cor(numericCols, use = "na.or.complete")
highCorrMatrix <- findCorrelation(x = corrMatrix, cutoff = .9)
#finCorrelationMatrix returns indexes relatice to numericCols. need to extract the col names so we can subset the original training dataset

highCorrNames <- names(numericCols[,highCorrMatrix]); 
highCorrNames <- names(training) %in% highCorrNames

training <- training[!highCorrNames]

###########
#train using the randomForest library. caret package has some overhead for this use case
rf <- randomForest(data=training, x = training[,-46], y=training$classe, 
                   importance = TRUE, replace = TRUE, )
pp <- predict(modFit, testing)
table(pp, testing$classe)
summary(testing$classe == pp)

#########################################
#print("Runnning rf now")
#trc <- trainControl(method="cv", number=5, returnData = FALSE, returnResamp = "none", savePredictions = FALSE)
#modFit <- train(classe ~ ., data = training, trainControl = trc, method="rf", prox=TRUE, model=FALSE)
#################################################

################
#trc <- trainControl(method="repeatedcv", repeats=5, number = 10)
#modFit <- train(classe ~ ., data=training, method="gbm", trControl = trc, verbose=TRUE)
##############

#remove cols that are duplicated - verify that cvtd_timestamp is duplicate

#notice a bunch of cols that have NA. we need to impute the data based on similar rows. the k nearest neighbors algo does this for us and is part of the caret package
#preProcObj <- preProcess(x=numericCols, method=c("knnImpute", "pca"))

#print("Runnning rf now")

#modFit <- train(training$classe ~ ., x = preProcObj$data, preProcess = preProcObj, method = "rf")
#train(training$classe ~ ., x = preProcObj$data, preProcess = preProcObj, method = "rf")



testdata <- read.csv("T:/dev/ws/DataScience/PracticalMachineLearning/PredictingWithTrees/PracticalMachineLearning/PracticalMachineLearning-Scratch-Pad/project/pml-testing.csv", na.strings = c("NA", ""))
