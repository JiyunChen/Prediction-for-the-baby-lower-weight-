rm(list=ls())
library(randomForest)
library(ROCR)
a <- "factor"; n <- "numeric"
vartype <-
c(a,a,a,n,n,n,n,n,a,n,a,a,a,a,a,n,a,n,a,n,a,n,a,a,a,a,a,a,n,a,a,a,a,a,n,n,a,a,a,a,a,n,a,a,a,a,a,
  n,n,n,n,a,a,a,a,a,a,n,a,n,a,n,a)
z=read.table(file.choose(),header = TRUE,colClasses = vartype)
z$wt<-NULL
for (i in 1:ncol(z)) {
  if (class(z[, i]) == "numeric") {
    index = which(is.na(z[, i]))
    if (length(index) == 0) {
      next
    } else{
      z[index, i] <- median(z[, i][-index])
    }
  }
}
traindata=z[1:20000,]
testdata<-z[-(1:100000),]
Rode for method "svm" in R
set.seed(1234)
n <- length(names(traindata))
for (i in 1:(n-1)){
  model <- randomForest(lowbwt~., data = traindata, mtry = i, ntree=100)
  err <- mean(model$err.rate)
  print(err)
}
rf_ntree <- randomForest(lowbwt~.,data=traindata,ntree=300,mtry=13)#
rf_ntree <- grow(rf_ntree,100)
predict=predict(rf_ntree,data=traindata)
rf_ntree
plot(rf_ntree)
names(predict)<-NULL
pre=prediction(as.integer(predict),labels = as.integer(traindata$lowbwt))
per<-performance(pre,"tpr","fpr" )
auc <- performance(pre, "auc")@y.values
auc
sum=0;index<-c();j=1
for(i in 1:100000){
  if((as.integer(traindata$lowbwt)[i]-as.integer(predict)[i])==0){
    sum=0+sum
  }
  else{
    if(as.integer(predict)[i]==1){
      sum=sum+10
      index[j]=i
      j=j+1
    }
    else{
      sum=sum+1
    }
  }
}
sum/100000