limitedTrain = dtmframeTrain
limitedTrain$sold = NULL
limitedTest = testframe
--limitedTest$PositiveDec = NULL

limitedTrain$Astartprice<-as.numeric(as.character(limitedTrain$Astartprice))

limitedTrain$Acondition<-as.numeric(limitedTrain$Acondition)
limitedTrain$Acellular<-as.numeric(limitedTrain$Acellular)
limitedTrain$Acarrier<-as.numeric(limitedTrain$Acarrier)
limitedTrain$Acolor<-as.numeric(limitedTrain$Acolor)
limitedTrain$Astorage<-as.numeric(limitedTrain$Astorage)
limitedTrain$Aproductline<-as.numeric(limitedTrain$Aproductline)

limitedTest$Acondition<-as.numeric(limitedTest$Acondition)
limitedTest$Acellular<-as.numeric(limitedTest$Acellular)
limitedTest$Acarrier<-as.numeric(limitedTest$Acarrier)
limitedTest$Acolor<-as.numeric(limitedTest$Acolor)
limitedTest$Astorage<-as.numeric(limitedTest$Astorage)
limitedTest$Aproductline<-as.numeric(limitedTest$Aproductline)







library(caret)

preproc = preProcess(limitedTrain)

normTrain = predict(preproc, limitedTrain)

normTest = predict(preproc, limitedTest)

kosDist = dist(normTrain, method="euclidean")

kosHierClust = hclust(kosDist, method="ward.D")
plot(kosHierClust)

set.seed(144)
k<-6
km<-kmeans(normTrain,centers=k)
str(km)

library(flexclust)

km.kcca = as.kcca(km, normTrain)

clusterTrain = predict(km.kcca)

clusterTest = predict(km.kcca, newdata=normTest)

itemstrain1<-subset(dtmframeTrain,clusterTrain==1)
itemstrain2<-subset(dtmframeTrain,clusterTrain==2)
itemstrain3<-subset(dtmframeTrain,clusterTrain==3)
itemstrain4<-subset(dtmframeTrain,clusterTrain==4)
itemstrain5<-subset(dtmframeTrain,clusterTrain==5)
itemstrain6<-subset(dtmframeTrain,clusterTrain==6)

summary((itemstrain1$sold))
summary((itemstrain2$sold))
summary((itemstrain3$sold))
summary((itemstrain4$sold))
summary((itemstrain5$sold))
summary((itemstrain6$sold))

library(rpart)
itemsmodel1<-rpart(sold~.,data=itemstrain1,method="class")
itemsmodel2<-rpart(sold~.,data=itemstrain2,method="class")
itemsmodel3<-rpart(sold~.,data=itemstrain3,method="class")
itemsmodel4<-rpart(sold~.,data=itemstrain4,method="class")
itemsmodel5<-rpart(sold~.,data=itemstrain5,method="class")
itemsmodel6<-rpart(sold~.,data=itemstrain6,method="class")

pred1<-predict(itemsmodel1)
pred2<-predict(itemsmodel2)
pred3<-predict(itemsmodel3)
pred4<-predict(itemsmodel4)
pred5<-predict(itemsmodel5)
pred6<-predict(itemsmodel6)

AllPredictions = c(pred1, pred2, pred3,pred4,pred5,pred6)

AllOutcomes = c(itemstrain1$sold,itemstrain2$sold,itemstrain3$sold,itemstrain4$sold,itemstrain5$sold,itemstrain6$sold)
table(AllOutcomes,AllPredictions>0.5)


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
