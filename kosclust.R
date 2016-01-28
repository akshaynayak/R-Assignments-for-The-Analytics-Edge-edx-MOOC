kosDist = dist(dailykos, method="euclidean")

kosHierClust = hclust(kosDist, method="ward.D")
plot(kosHierClust)

rect.hclust(kosHierClust, k = 7, border = "red")
kosClusters = cutree(kosHierClust, k = 7)
str(kosClusters)

s1<-subset(dailykos,kosClusters==1)
s2<-subset(dailykos,kosClusters==2)
s3<-subset(dailykos,kosClusters==3)
s4<-subset(dailykos,kosClusters==4)
s5<-subset(dailykos,kosClusters==5)
s6<-subset(dailykos,kosClusters==6)
s7<-subset(dailykos,kosClusters==7)

tail(sort(colMeans(s1)))
tail(sort(colMeans(s2)))
tail(sort(colMeans(s3)))
tail(sort(colMeans(s4)))
tail(sort(colMeans(s5)))
tail(sort(colMeans(s6)))
tail(sort(colMeans(s7)))

-----------
  # Specify number of clusters
  k = 7

# Run k-means
set.seed(1000)
KMC = kmeans(dailykos, centers = k)
str(KMC)

kosclus<-KMC$cluster
str(kosclus)
summary(as.factor(kosclus))
"table(hierGroups, KmeansCluster$cluster)"
