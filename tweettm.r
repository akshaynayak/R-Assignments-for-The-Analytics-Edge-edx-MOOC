tweets$negative<-as.factor(tweets$Avg<=-1)
table(tweets$negative)
library(tm)
library(SnowballC)
length(stopwords("english"))
corpus<-Corpus(VectorSource(tweets$Tweet))
corpus
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,PlainTextDocument)

corpus[[1]]
corpus<-tm_map(corpus,removePunctuation)
stopwords("english")[1:10]
corpus<-tm_map(corpus,removeWords,c("apple",stopwords("english")))
corpus<-tm_map(corpus,stemDocument)

dtm<-DocumentTermMatrix(corpus)
dtm
findFreqTerms(dtm,lowfreq=100)
outputsparse<-removeSparseTerms(dtm,0.995)
outputsparse
colnames(outputsparse)
tweetssparse<-as.data.frame(as.matrix(outputsparse))

colnames(tweetssparse)<-make.names(colnames(tweetssparse))
tweetssparse$negative<-tweets$negative
library(caTools)
set.seed(123)
spl<-sample.split(tweetssparse$negative,SplitRatio=0.7)
train<-subset(tweetssparse,spl==TRUE)
test<-subset(tweetssparse,spl==FALSE)

library(rpart)
tweetcart<-rpart(negative~.,data=train,method="class")
summary(tweetcart)
tpred<-predict(tweetcart,newdata=test,type="class")


table(test$negative,tpred)
library(randomForest)
set.seed(123)
tweetrf<-randomForest(negative~.,data=train,method="class")
tpred<-predict(tweetrf,newdata=test,type="class")

-----logistic
tlog<-glm(negative~.,data=train,family="binomial")
predictions = predict(tlog, newdata=test, type="response")
table(test$negative,predictions>0.5)


