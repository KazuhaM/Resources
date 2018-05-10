read.table("relative.txt",header=T)->relative
relative
plot(prop~age,relative,xlim=c(0,70),ylim=c(0,80))
relative.age <- lm(prop~age,data=relative)
summary(relative.age)
abline(a=10.96,b=0.76,col="red")

lines(prop~age,subset(relative,sex=="F"),type="b",col="red")
lines(prop~age,subset(relative,sex=="M"),type="b",col="blue")


relative.age2 <- lm(prop~age+sex,data=relative)
summary(relative.age2)


abline(a=19.7,b=0.75,col="red",lty=2)
abline(a=19.7-17.4,b=0.75,col="blue",lty=2)


relative.age3 <- lm(prop~(age+sex)^2,data=relative)
summary(relative.age3)

abline(a=12.6,b=0.9,col="red",lty=3)
abline(a=12.6-3.3,b=0.91-0.3,col="blue",lty=3)
abline(h=0,v=0)


read.csv("rice.csv",header=T)->rice
head(rice)
plot(gy~fert,rice)
summary(aov(gy~fert,rice))

summary(lm(gy~fert,rice))
rice$year0 <- as.factor(rice$year)
summary(lm(gy~year0,rice))
plot(gy~year0,rice)

summary(lm(gy~fert+year0,rice))



read.table("exercise7.txt",header=T)->exercise

exercise1 <- lm(y~x1,data=exercise)
summary(exercise1)

exercise2 <- lm(y~x1+x2,data=exercise)
summary(exercise2)


