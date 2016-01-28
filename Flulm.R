str(FluTrain)
maxvisitweek<-FluTrain[FluTrain$ILI==max(FluTrain$ILI),]
maxvisitweek

maxqueryweek<-FluTrain[FluTrain$Queries==max(FluTrain$Queries),]
maxqueryweek
hist(FluTrain$ILI)
plot(log(FluTrain$ILI),FluTrain$Queries)

flulm<-lm(log(ILI)~Queries,data=FluTrain)
summary(flulm)

PredTest1<-exp(predict(flulm,FluTest))
PredTest1
?which

SSE<-sum((PredTest1-FluTest$ILI)^2)
RMSE<-sqrt(SSE/nrow(FluTest))

ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)

FluTrain$ILILag2 = coredata(ILILag2)

plot(log(FluTrain$ILILag2),log(FluTrain$ILI))
FluTrend2<-lm(log(ILI)~Queries+log(ILILag2),data=FluTrain)
summary(FluTrend2)

ILILag2 = lag(zoo(FluTest$ILI), -2, na.pad=TRUE)

FluTest$ILILag2 = coredata(ILILag2)
summary(FluTest$ILILag2)

FluTest$ILILag2[1] = FluTrain$ILI[416]
FluTest$ILILag2[2] = FluTrain$ILI[417]

TestPred2<-exp(predict(FluTrend2,FluTest))

SSE<-sum((TestPred2-FluTest$ILI)^2)
RMSE<-sqrt(SSE/nrow(FluTest))
RMSE
