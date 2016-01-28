library(caret)
set.seed(325)
inTrain <- createDataPartition(y=unskewed_data$class_label,
                               p=0.7, list=FALSE)
library("e1071")

##svm
feature_matrix<-dtm2[inTrain,]
y_label<-unskewed_data$class_label[inTrain]
#model <- svm(class_label ~ ., data = dtm_frame_label)
y_label<-factor(y_label)
model<-svm(feature_matrix,y_label,cost=1,kernel="linear")
x