summary(as.factor(gerber$voting))
cd<-sum(gerber$voting==1&gerber$civicduty==1)
cd
ht<-sum(gerber$voting==1&gerber$hawthorne==1)
ht
s<-sum(gerber$voting==1&gerber$self==1)
s

f<-sum(gerber$voting==1&gerber$neighbors==1)
f

votinglog<-glm(voting~civicduty+hawthorne+self+neighbors,data=gerber,family=binomial)
summary(votinglog)
Predicted<-predict(votinglog,type="response")
table(gerber$voting,Predicted>0.5)

library(ROCR)
ROCRpred<-prediction(Predicted,gerber$voting)
as.numeric(performance(ROCRpred,"auc")@y.values)
---trees
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)

CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
CARTmodel3 = rpart(voting ~ sex+civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)

controlog<-rpart(voting~control,data=gerber,cp=0.0)
summary(controlog)
controlsexlog<-rpart(voting~control+sex,data=gerber,cp=0.0)
summary(controlsexlog)

LogModelSex<-glm(voting~sex+control,data=gerber)
summary(LogModelSex)


Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogModelSex, newdata=Possibilities, type="response")

--female and control
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

predict(LogModel2, newdata=Possibilities, type="response")
