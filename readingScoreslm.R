tapply(pisa2009train$readingScore,pisa2009train$male,mean)

newpisatrain<-pisa2009train[,colSums(is.na(pisa2009train))==0]
colnames(pisa2009train)

str(newpisatrain)


pisaTrain = na.omit(pisa2009train)

pisaTest = na.omit(pisa2009test)
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")

pisaTest$raceeth = relevel(pisaTest$raceeth, "White")

lmScore<-lm(readingScore~.,data=pisaTrain)
summary(lmScore)

SSE<-lmScore$residuals^2
RMSE<-sqrt(sum(SSE)/nrow(pisaTrain))
RMSE

predTest<-predict(lmScore,pisaTest)
summary(predTest)

SSE<-sum((predTest-pisaTest$readingScore)^2)
SSE
RMSE<-sqrt(SSE/nrow(pisaTest))
RMSE
baselinepred<-mean(pisaTrain$readingScore)
baselinepred
SST<-sum((baselinepred-pisaTest$readingScore)^2)
SST
rsquare<-1-(SSE/SST)
rsquare
