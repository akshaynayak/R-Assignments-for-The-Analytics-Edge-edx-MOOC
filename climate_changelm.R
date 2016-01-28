climate_train<-climate_change[climate_change$Year<2007,]
climate_test<-climate_change[climate_change$Year>=2007,]

rm(climate_train)
str(climate_change)
summary(climate_train$Year)

model<-lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=climate_train )
summary(model)
cor()
model2<-lm(Temp~ MEI+TSI+Aerosols+N2O,data=climate_train)
summary(model2)
step_model<-step(model)
summary(step_model)
test_predictions<-predict(step_model,climate_test)
SSE<-sum((test_predictions-climate_test$Temp)^2)
SST<-sum((mean(climate_train$Temp)-climate_test$Temp)^2)
r2<-1-(SSE/SST)
r2