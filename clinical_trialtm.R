max(nchar(clinical_trial$abstract))
clinical_trial[(nchar(clinical_trial$title)==28),]
library(tm)
corpustitle<-Corpus(VectorSource(clinical_trial$title))
corpusabstract<-Corpus(VectorSource(clinical_trial$abstract))

corpustitle = tm_map(corpustitle, tolower)
corpusabstract = tm_map(corpusabstract, tolower)

corpustitle = tm_map(corpustitle, PlainTextDocument)
corpusabstract = tm_map(corpusabstract, PlainTextDocument)

corpustitle<-tm_map(corpustitle,removePunctuation)
corpusabstract<-tm_map(corpusabstract,removePunctuation)

corpustitle<-tm_map(corpustitle,removeWords,stopwords("english"))
corpusabstract<-tm_map(corpusabstract,removeWords,stopwords("english"))

corpustitle<-tm_map(corpustitle,stemDocument)
corpusabstract<-tm_map(corpusabstract,stemDocument)

dtmtitle<-DocumentTermMatrix(corpustitle)
dtmabstract<-DocumentTermMatrix(corpusabstract)

dtmtitle<-removeSparseTerms(dtmtitle,0.95)
dtmabstract<-removeSparseTerms(dtmabstract,0.95)

dtmtitle<-as.data.frame(as.matrix(dtmtitle))
dtmabstract<-as.data.frame(as.matrix(dtmabstract))

colnames(dtmabstract)[(colSums(dtmabstract))==max(colSums(dtmabstract))]
colnames(dtmtitle) = paste0("T", colnames(dtmtitle))

colnames(dtmabstract) = paste0("A", colnames(dtmabstract))

dtm = cbind(dtmtitle, dtmabstract)
dtm$trial<-clinical_trial$trial

set.seed(144)
library(caTools)
spl<-sample.split(dtm$trial,SplitRatio=0.7)
train<-subset(dtm,spl==TRUE)
test<-subset(dtm,spl==FALSE)
summary(as.factor(test$trial))
library(rpart)
trialcart<-rpart(trial~.,data=train,method="class")
summary(trialcart)
trainpred<-predict(trialcart)
max(trainpred[,1])
str(trainpred)
table(train$trial,trainpred[,2]>0.5)
testpred<-predict(trialcart,newdata=test)
table(test$trial,testpred)
library(ROCR)
ROCRpred<-prediction(testpred[,2],test$trial)
as.numeric(performance(ROCRpred,"auc")@y.values)
