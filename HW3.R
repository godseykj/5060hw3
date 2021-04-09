BG_data=BGSamplingFrame2019version2
library(raster)
sqft=BG_data$SqFootage
cv(sqft,na.rm = TRUE)
YearBuilt=BG_data$YearBuilt
cv(YearBuilt,na.rm = TRUE )
Totacreage=BG_data$totACRES
cv(Totacreage,na.rm=TRUE)
NumRooms=BG_data$NumRooms
cv(NumRooms,na.rm = TRUE)
Numbedrooms=BG_data$NumBeds
cv(Numbedrooms,na.rm=TRUE)
Numbathrooms=BG_data$NumBaths
cv(Numbathrooms,na.rm=TRUE)
# Fiitting the data into linear models
Model1<-lm(BG_data$SqFootage~BG_data$YearBuilt)
Model2<-lm(BG_data$SqFootage~BG_data$totACRES)
Model3<-lm(BG_data$SqFootage~BG_data$NumRooms)
Model4<-lm(BG_data$SqFootage~BG_data$NumBeds)
Model5<-lm(BG_data$SqFootage~BG_data$NumBaths)

#Plot of the auxiliary variables against the variable Sqft
par(mfrow=c(2,3))
plot(BG_data$YearBuilt,BG_data$SqFootage, main="Scatterplot of Yearbuilt vs.Sqft")
abline(Model1) 
legend("topleft",legend=paste("R2 is", format(summary(Model1)$r.squared,digits=6))) 

plot(BG_data$totACRES,BG_data$SqFootage, main="Scatterplot of Totacres vs.Sqft")
abline(Model2) 
legend("topleft",legend=paste("R2 is", format(summary(Model2)$r.squared,digits=6)))

plot(BG_data$NumRooms,BG_data$SqFootage, main="Scatterplot of Numrooms vs.Sqft")
abline(Model3) 
legend("topleft",legend=paste("R2 is", format(summary(Model3)$r.squared,digits=6)))

plot(BG_data$NumBeds,BG_data$SqFootage, main="Scatterplot of Numbeds vs.Sqft")
abline(Model4) 
legend("topleft",legend=paste("R2 is", format(summary(Model4)$r.squared,digits=6)))

plot(BG_data$NumBaths,BG_data$SqFootage, main="Scatterplot of Numbaths vs.Sqft")
abline(Model5) 
legend("topleft",legend=paste("R2 is", format(summary(Model5)$r.squared,digits=6)))

#Calculating the confindence interval for the intercepts for the respective models

confint(Model1,level=.95)
confint(Model2,level=.95)
confint(Model3,level=.95)
confint(Model4,level=.95)
confint(Model5,level=.95)

