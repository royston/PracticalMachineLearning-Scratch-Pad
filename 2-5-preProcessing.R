library(caret);

#spam data set is in the kernlab library
library(kernlab)
data(spam)

#split it into training and test sets
#split based on the type outcome with 75% to training set
inTrain <- createDataPartition(y=spam$type, p=0.75, list=FALSE)


training <- spam[inTrain,]
testing <- spam[-inTrain,]

#Preprocessing

#2. Standardization :
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
sd(trainCapAveS) # gives 

#you can then use the object created by preProcessing technique in R and use it on the test set
testCapAveS <- predict(preObj, testing[,-58])$capitalAve
mean(testCapAveS)
sd(testCapAveS) #note that mean and sd will not be exatly 0 and 1, because the pre proc was done on training data


#2. Pre processing using the box cox transforms
# take continuous data and try to make them look like normal data - not sure what the means, read up later
preObj <- preProcess(training[,-58], method=c("BoxCox")) 
trainCapAveS <- predict(preObj, training[,-58])$capitalAve
par(mfrow=c(1,2))
hist(trainCapAveS)
qqnorm(trainCapAveS)


#3.  Imputing data - We might also need to impute data where some data is missing
#you can use knn Impute - k nearest neighbors imputation: 
#         find the k nearest data vectors that look most like the missing vectors, average the missing values and impute them

# for demonstation, to setup the data for this, lets insert NA in some rows in the capitalAve col randomly
training$capAve <- training$capitalAVe
selectNA <- rbinom(dim(training)[1], size=1, prob=0.05)==1
training$capiAve[selectNA] <- NA

#Impute and standardize
preObj <- preProcess(training[,-58], method="knnImpute")
capAve <- predict(preObj, training[,-58])$capAve
#then standardize the true values
capAveTruth <- training$capitalAve
capAveTruth <- (capAveTruth - mean(capAveTruth))/sd(capAveTruth)

