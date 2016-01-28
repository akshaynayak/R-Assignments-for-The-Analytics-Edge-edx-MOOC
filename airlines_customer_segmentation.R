summary(AirlinesCluster$QualMiles)

library(caret)
preproc = preProcess(AirlinesCluster)

airlinesNorm = predict(preproc, AirlinesCluster)

distance<-dist(airlinesNorm,method="euclidean")
hcluster<-hclust(distance,method="ward.D")
plot(hcluster)

customer_cluster<-cutree(hcluster,k=5)
str(customer_cluster)
table(customer_cluster)

********-----imp step to identify the type of customers------***********
tapply(AirlinesCluster$Balance, customer_cluster, mean)

k<-5
set.seed(88)
KMC<-kmeans(airlinesNorm,centers=k,iter.max=1000)
str(KMC)
