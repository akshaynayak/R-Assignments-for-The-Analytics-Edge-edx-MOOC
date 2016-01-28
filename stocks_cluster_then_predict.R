set.seed(144)
library(caTools)

spl = sample.split(StocksCluster$PositiveDec, SplitRatio = 0.7)

stocksTrain = subset(StocksCluster, spl == TRUE)

stocksTest = subset(StocksCluster, spl == FALSE)

StocksModel<-glm(PositiveDec~.,data=stocksTrain,family="binomial")
trainpred<-predict(StocksModel,type="response")
table(stocksTrain$PositiveDec,trainpred>0.5)

testpred<-predict(StocksModel,newdata=stocksTest,type="response")
table(stocksTest$PositiveDec,testpred>0.5)

limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

set.seed(144)
k<-3
km<-kmeans(normTrain,centers=k)
str(km)

library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

stockstrain1<-subset(stocksTrain,clusterTrain==1)
stockstrain2<-subset(stocksTrain,clusterTrain==2)
stockstrain3<-subset(stocksTrain,clusterTrain==3)

summary((stockstrain1$PositiveDec))
summary((stockstrain2$PositiveDec))
summary((stockstrain3$PositiveDec))

stocksmodel1<-glm(PositiveDec~.,data=stockstrain1,family="binomial")
stocksmodel2<-glm(PositiveDec~.,data=stockstrain2,family="binomial")
stocksmodel3<-glm(PositiveDec~.,data=stockstrain3,family="binomial")

stockstest1<-subset(stocksTest,clusterTest==1)
stockstest2<-subset(stocksTest,clusterTest==2)
stockstest3<-subset(stocksTest,clusterTest==3)

testpred1<-predict(stocksmodel1,newdata=stockstest1,type="response")
table(stockstest1$PositiveDec,testpred1>0.5)

testpred2<-predict(stocksmodel2,newdata=stockstest2,type="response")
table(stockstest2$PositiveDec,testpred2>0.5)

testpred3<-predict(stocksmodel3,newdata=stockstest3,type="response")
table(stockstest3$PositiveDec,testpred3>0.5)

AllPredictions = c(testpred1, testpred2, testpred3)

AllOutcomes = c(stockstest1$PositiveDec, stockstest2$PositiveDec, stockstest3$PositiveDec)
table(AllOutcomes,AllPredictions>0.5)

