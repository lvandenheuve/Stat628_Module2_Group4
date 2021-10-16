## data cleaning
data=read.csv("BodyFat.csv",header=TRUE)
colnames(data)

boxplot(data$BODYFAT)

boxplot(data$HEIGHT)

boxplot(data$WEIGHT)



## modeling
data=read.csv("BodyFat3.0.csv",header=TRUE)
data
colnames(data)

model1=lm(BODYFAT~Abdomen.Inch.+WEIGHT+Abdomen.Inch.*WEIGHT,data=data)
summary(model1)
fitted_values=model1$fitted.values
residuals=model1$residuals
plot(fitted_values,residuals)

qqnorm(residuals, pch = 1, frame = FALSE)
qqline(residuals, col = "steelblue", lwd = 2)

model2=lm(BODYFAT~Abdomen.Inch.+ADIPOSITY,data=data)
summary(model2)

model3=lm(BODYFAT~Abdomen.Inch.+Chest.Inch.,data=data)
summary(model3)

model4=lm(BODYFAT~Abdomen.Inch.+Hip.Inch.,data=data)
summary(model4)

cbind(CIlower =  2.860768 - 1.96*0.224595, CIupper = 2.860768 + 1.96*0.224595)

cbind(CIlower =  -0.014148 - 1.96*0.045908, CIupper = -0.014148 + 1.96*0.045908)

cbind(CIlower =  -0.003050 - 1.96*0.001002, CIupper = -0.003050 + 1.96*0.001002)


cor(data)

par(mfrow=c(4,3))
##scatter plot
plot(data$DENSITY,data$BODYFAT)
plot(data$AGE,data$BODYFAT)
plot(data$WEIGHT,data$BODYFAT) 
plot(data$HEIGHT,data$BODYFAT)
plot(data$NECK,data$BODYFAT)
plot(data$CHEST,data$BODYFAT)
plot(data$Abdomen.Inch.,data$BODYFAT)
plot(data$HIP,data$BODYFAT)
plot(data$THIGH,data$BODYFAT)
plot(data$KNEE,data$BODYFAT)
plot(data$ANKLE,data$BODYFAT)
plot(data$BICEPS,data$BODYFAT)


cor(data$DENSITY,data$BODYFAT)
cor(data$AGE,data$BODYFAT)
cor(data$WEIGHT,data$BODYFAT)
cor(data$HEIGHT,data$BODYFAT)
cor(data$NECK,data$BODYFAT)
cor(data$CHEST,data$BODYFAT)
cor(data$Abdomen.Inch.,data$BODYFAT)
cor(data$HIP,data$BODYFAT)
cor(data$THIGH,data$BODYFAT)
cor(data$KNEE,data$BODYFAT)
cor(data$ANKLE,data$BODYFAT)
cor(data$BICEPS,data$BODYFAT)