library(caTools)
set.seed(3000)
spl<-sample.split(stevens$Reverse,SplitRatio=0.7)
Train<-subset(stevens,spl==TRUE)
Test<-subset(stevens,spl==FALSE)

library(rpart)
library(rpart.plot)
StevensTree<-rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,method="class",minbucket=25)
prp(StevensTree)
PredictCart<-predict(StevensTree,newdata=Test,type="class")
table(Test$Reverse,PredictCart)

library(ROCR)
PredictROC<-predict(StevensTree,newdata=Test)
PredictROC

pred<-prediction(PredictROC[,2],Test$Reverse)
perf<-performance(pred,"tpr","fpr")
plot(perf)

as.numeric(performance(pred, "auc")@y.values)

---
StevensTree<-rpart(Reverse~Circuit+Issue+Petitioner+Respondent+LowerCourt+Unconst,data=Train,method="class",minbucket=5)
summary(StevensTree)
prp(StevensTree)

library(randomForest)
set.seed(200)
StevensForest<-randomForest(Reverse~Circuit+Issue+Petitioner+LowerCourt+Unconst,data=Train,nodesize=25,ntrees=200)
Train$Reverse<-as.factor(Train$Reverse)
Test$Reverse<-as.factor(Test$Reverse)
PredictForest<-predict(StevensForest,newdata=Test)
table(Test$Reverse,PredictForest)

library(caret)
library(e1071)
numFolds<-trainControl(method="cv",number=10)
cpGrid<-expand.grid(.cp=seq(0.01,0.5,0.01))

rpartmodel<-train(as.factor(Reverse)~Circuit+Issue+Petitioner+LowerCourt+Unconst,data=Train,method="rpart",trContol = numFolds,tuneGrid=cpGrid)

