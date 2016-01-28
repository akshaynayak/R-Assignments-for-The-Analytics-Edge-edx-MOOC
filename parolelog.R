summary(as.factor(parole$violator))
factstate<-as.factor(parole$state)
factcrime<-as.factor(parole$crime)
parole$state<-factstate
parole$crime<-factcrime

set.seed(144)
library(caTools)
split<-sample.split(parole$violator,SplitRatio=0.7)
train<-subset(parole,split==TRUE)
test<-subset(parole,split==FALSE)

parolelog<-glm(violator~.,data=train,family=binomial)
summary(parolelog)

newparolee<-c(1,1,50,1,3,12,0,2)
sampredict<-predict(parolelog,type="response",newdata=test)
max(sampredict)
table(test$violator,sampredict>0.5)
library(caret)
confusionmatrix(test$violator,samplepredict>0.5)

library(ROCR)
ROCRpred<-prediction(sampredict,test$violator)
as.numeric(performance(ROCRpred,"auc")@y.values)


