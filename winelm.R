model1<-lm(Price~HarvestRain+WinterRain,data=wine)
model1
summary(model1)
str(wine)

cor(wine$HarvestRain,wine$WinterRain)
