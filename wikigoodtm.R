wiki$Vandal<-as.factor(wiki$Vandal)
table(wiki$Vandal)
library(tm)
corpusadded<-Corpus(VectorSource(wiki$Added))
corpusa<-tm_map(corpusadded,removeWords,stopwords("english"))
corpusa<-tm_map(corpusa,stemDocument)
dtmadded<-DocumentTermMatrix(corpusa)
dtmadded
sparseadded<-removeSparseTerms(dtmadded,0.997)
sparseadded
wordsadded<-as.data.frame(as.matrix(sparseadded))
colnames(wordsadded) = paste("A", colnames(wordsadded))

------
corpusremoved<-Corpus(VectorSource(wiki$Removed))
corpusr<-tm_map(corpusremoved,removeWords,stopwords("english"))
corpusr<-tm_map(corpusr,stemDocument)
dtmremoved<-DocumentTermMatrix(corpusr)
dtmremoved
sparseremoved<-removeSparseTerms(dtmremoved,0.997)
sparseremoved
wordsremoved<-as.data.frame(as.matrix(sparseremoved))
colnames(wordsremoved) = paste("A", colnames(wordsremoved))
wikiWords = cbind(wordsadded, wordsremoved)
wikiWords$Vandal<-wiki$Vandal
set.seed(123)
spl<-sample.split(wikiWords$Vandal,SplitRatio=0.7)
train<-subset(wikiWords,spl==TRUE)
test<-subset(wikiWords,spl==FALSE)
table(test$Vandal)
wikicart<-rpart(Vandal~.,data=train,method="class")
wikipred<-predict(wikicart,newdata=test,type="class")
table(test$Vandal,wikipred)
----
  wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
sum(wikiWords2$HTTP==1)
wikiTrain2 = subset(wikiWords2, spl==TRUE)

wikiTest2 = subset(wikiWords2, spl==FALSE)
wikicart2<-rpart(Vandal~.,data=wikiTrain2,method="class")
wikipred2<-predict(wikicart2,newdata=wikiTest2,type="class")
table(wikiTest2$Vandal,wikipred2)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmadded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmremoved))
mean(wikiWords2$NumWordsAdded)
-----
  wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor

wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3, spl==TRUE)

wikiTest3 = subset(wikiWords3, spl==FALSE)
wikicart3<-rpart(Vandal~.,data=wikiTrain3,method="class")
wikipred3<-predict(wikicart3,newdata=wikiTest3,type="class")
table(wikiTest3$Vandal,wikipred3)
