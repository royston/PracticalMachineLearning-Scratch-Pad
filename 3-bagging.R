library(ElemStatLearn);
data(ozone, package="ElemStatLearn")


#order by the ozone col ->ozone is also a column name in this dataset
ozone <- ozone[order(ozone$ozone),]


ll <- matrix(NA, nrow=10, ncol=155)
#loop through the data 10 times so you can resample with replacement
for(i in 1:10){
  ss <- sample(1:dim(ozone)[1], replace=T)
  ozone0 <- ozone[ss,];
  ozone0 <- ozone0[order(ozone0$ozone),];
  
  #fit a smooth curve fitting temperature vs ozone with the new data
  loess0 <- loess(temperature ~ ozone, data=ozone0, span=.2);
  ll[i,] <- predict(loess0, newdata=data.frame(ozone=1:155));
}

plot(ozone$ozone, ozone$temperature, pch=19, cex=0.5)

for(i in 1:10)