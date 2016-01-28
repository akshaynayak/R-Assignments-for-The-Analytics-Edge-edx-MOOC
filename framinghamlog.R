library(caTools)
split<-sample.split(framingham$TenYearCHD,SplitRatio=0.65)
train<-subset(framingham,split==TRUE)
test<-subset(framingham,split==FALSE)
framinghamLog<-glm(TenYearCHD~.,data=train,family=binomial)
predictTest<-predict(framinghamLog,type="response",newdata=test)
library(ROCR)
ROCRpred<-prediction(predictTest,test$TenYearCHD)
as.numeric(performance(ROCRpred,"auc")@y.values)


