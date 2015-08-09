library(caret);

#spam data set is in the kernlab library
library(kernlab)
data(spam)

#split it into training and test sets
#split based on the type outcome with 75% to training set
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)


training <- spam[inTrain,]
testing <- spam[-inTrain,]


#Other way to split data is using folds. in this case 10 folds
    #set.seed(32323)
    #folds <- createFolds(y=spam$type, k=10, list=TRUE, returnTrain = TRUE)


#Fitting a model to the data
#train using the glm method
#modelFit <- train(type ~. data=training, method = "glm" )


#Preprocessing

#Standardization :
#When variable values are skewed - when some outliers differ greatly from the mean, you have a high standard deviation
# It is important to standardize the variables so the ML algos do no get tricked
# eg in the spam dataset, the capitalAve is highly skewed
hist(training$capitalAve, main="", xlab = "ave. capital run length")
mean(training$capitalAve) # gives around 4.5
sd(training$capitalAve) # gives you around 34, which is high

#pre process using the proProcess function
preObj <- preProcess(training[,-58], method=c("center", "scale")) #center the variables to the mean and scale them to reduce standard deviation
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
mean(trainCapAveS) # gives almost 0
sd(trainCapAveS) # gives 1











