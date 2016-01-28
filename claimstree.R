library(caTools)
set.seed(88)
spl<-sample.split(ClaimsData$bucket2009,SplitRatio=0.6)
ClaimsTrain<-subset(ClaimsData,spl==TRUE)
ClaimsTest<-subset(ClaimsData,spl==FALSE)
mean(ClaimsTrain$age)

sum(ClaimsTrain$diabetes==1)

penaltymatrix<-matrix(c(0,1,2,3,4,2,0,1,2,3,4,2,0,1,2,6,4,2,0,1,8,6,4,2,0),byrow=TRUE,nrow=5)
penaltymatrix
sum(as.matrix(table(ClaimsTest$bucket2009,ClaimsTest$bucket2008))*penaltymatrix)/nrow(ClaimsTest)

sum(table(ClaimsTest$bucket2009,rep(1,each=183202))*as.matrix(c(0,2,4,6,8),byrow=TRUE,nrow=5))/nrow(ClaimsTest)

library(rpart)
ClaimsTree<-rpart(bucket2009~age+arthritis+alzheimers+cancer+copd+depression+diabetes+heart.failure+ihd+kidney+osteoporosis+stroke+bucket2008+reimbursement2008,data=ClaimsTrain,method="class",cp=0.00005,parms=list(loss=penaltymatrix))
PredictTest<-predict(ClaimsTree,newdiata=ClaimsTest,type="class")
as.matrix(table(ClaimsTest$bucket2009,PredictTest))*penaltymatrix


sum(ClaimsTest$bucket2009==1)

summary(as.factor(ClaimsTest$bucket2009))

