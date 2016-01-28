census = read.csv("census.csv")
  
library(caTools)

set.seed(2000)

spl = sample.split(census$over50k, SplitRatio = 0.6)

train = subset(census, spl==TRUE)

test = subset(census, spl==FALSE)

censusglm = glm( over50k ~ . , family="binomial", data = train)

summary(censusglm)

-------------
  set.seed(1)

trainSmall = train[sample(nrow(train), 2000), ]
  
  To generate the random forest model with all of the variables, just run:
library(randomForest)

  set.seed(1)

censusrf = randomForest(over50k ~ . , data = trainSmall)

And then you can make predictions on the test set by using the following command:
  
  predictTest = predict(censusrf, newdata=test)

And to compute the accuracy, you can create the confusion matrix:
  
  table(test$over50k, predictTest)

The accuracy of the model should be around

(9614+1050)/nrow(test) = 0.8337112
----------
  vu = varUsed(censusrf, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(censusrf$forest$xlevels[vusorted$ix]))
varImpPlot(censusrf)
library(caret)
set.seed(2)
fitControl = trainControl( method = "cv", number = 10 )
cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
train( over50k ~ . , data = train, method = "rpart", trControl = fitControl, tuneGrid = cartGrid )
