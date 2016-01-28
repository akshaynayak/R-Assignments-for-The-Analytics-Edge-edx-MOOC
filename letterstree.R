letters_ABPR$isB = as.factor(letters_ABPR$letter == "B")
library(caTools)

set.seed(1000)
spl<-sample.split(letters_ABPR$isB,SplitRatio=0.5)
train<-subset(letters_ABPR,spl=TRUE)
test<-subset(letters_ABPR,spl=FALSE)
summary(test$isB)
library(rpart)

CARTb = rpart(isB ~ . - letter, data=train, method="class")
testpred<-predict(CARTb,newdata=test,type="class")
table(test$isB,testpred)

set.seed(1000)
library(randomForest)

rfisb<-randomForest(train$isB~.-letter,data=train)
testpred<-predict(rfisb,newdata=test,type="class")
table(test$isB,testpred)

-----
letters_ABPR$letter<-as.factor(letters_ABPR$letter)
set.seed(2000)
spl<-sample.split(letters_ABPR$letter,SplitRatio=0.5)
train<-subset(letters_ABPR,spl=TRUE)
test<-subset(letters_ABPR,spl=FALSE)
summary(test$letter)
CARTb = rpart(letter ~ . - isB, data=train, method="class")
testpred<-predict(CARTb,newdata=test,type="class")
table(test$letter,testpred)

set.seed(1000)
rfisb<-randomForest(train$letter~.-isB,data=train)
testpred<-predict(rfisb,newdata=test,type="class")
table(test$letter,testpred)

-
