library(tm)
corpus<-Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus<-tm_map(corpus,tolower)
corpus<-tm_map(corpus,PlainTextDocument)
corpus<-tm_map(corpus,removePunctuation)
corpus<-tm_map(corpus,removeWords,stopwords("english"))
corpus<-tm_map(corpus,stemDocument)
dtm<-DocumentTermMatrix(corpus)
dtmsparse<-removeSparseTerms(dtm,0.97)
labelledterms<-as.data.frame(as.matrix(dtmsparse))
labelledterms$responsive<-emails$responsive

library(caTools)
spl<-sample.split(labelledterms$responsive,SplitRatio=0.7)
train<-subset(labelledterms,spl==TRUE)
test<-subset(labelledterms,spl==FALSE)
emailcart<-rpart(responsive~.,data=train,method="class")

library(rpart)
epred<-predict(emailcart,newdata=test,type="class")
table(test$responsive,epred)

