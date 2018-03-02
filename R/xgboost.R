rm(list=ls())
library(data.table)
library(xgboost)
require(ROCR)
#step1: get the data type of the all variables
dsc <- readLines("dscfile.txt")
roles <- NULL
for(j in 4:length(dsc)){ # store original roles of variables
  strng <- strsplit(dsc[j]," +")[[1]]
  roles <- c(roles,strng[3])
}
vartype <- c()
for(i in 1:length(roles)){
  if(roles[i]=="c"){
    vartype[i]="factor"
  }
  else{
    vartype[i]="numeric"
  }
}
#step2: fix up the missing value with the median
z=read.table("2016_allvar_allyear.rdata",header = TRUE,colClasses = vartype)
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
dim(na.omit(z));z_all<-z
set.seed(0)
index=sample(1:(nrow(z)-18000),500000)
z<-z[index,]
dim(na.omit(z))
#step3: get the train data,sample sizes=50000
setDT(z)
traindata=z[1:400000,]
testdata=z[400001:500000,]
testdata=z[2236989:2254988,]
label_train=traindata$lowbwt
labels <- sign(as.numeric(label_train) ==2 )
xg_train <- model.matrix(~.+0,data = traindata[,-c("lowbwt"),with=F])
xg_test <- model.matrix(~.+0,data = testdata[,-c("lowbwt"),with=F])
model_xg <- xgboost(data = xg_train, label = labels, max.depth = 3, eta = 0.3,nround = 300,
                    objective = "binary:logistic")
predict_xg_train=predict(model_xg,xg_train)
#step4: calculate auc
label_test=sign(as.numeric(testdata$lowbwt) ==2 )
pre_train=prediction(predict_xg_train,labels = labels)
Rode for method "randomforest" in R
auc_train <- performance(pre_train, "auc")@y.values
auc_train
#step5: calculate the cost
#table(label_test);table(sign(predict_xg>=0.088))
cost_sum<-c();
rank=predict_xg_train[order(predict_xg_train,decreasing = T)]
for (i in 365557:dim(traindata)[1]){
  res <- sign(predict_xg_train>=rank[i])-labels
  ind_1 <- which(res == 1)
  ind_2 <- which(res ==-1)
  cost <- sum(rep(1,length(ind_1)))+sum(rep(10,length(ind_2)))
  cost_sum[i]<-cost/dim(traindata)[1]
  print(c(i,cost_sum[i]))
}
bound=predict_xg_train[which(cost_sum==min(cost_sum))]#0.08652855_40wan
#step6 predict
predict_xg_test=predict(model_xg,xg_test)
res <- sign(predict_xg_test>=bound)-label_test
ind_1 <- which(res == 1)
ind_2 <- which(res ==-1)
cost <- sum(rep(1,length(ind_1)))+sum(rep(10,length(ind_2)))
cost/dim(testdata)[1]
pre=prediction(sign(predict_xg_test>=bound),labels = label_test)
per<-performance(pre,"tpr","fpr" )
auc <- performance(pre, "auc")@y.values
auc