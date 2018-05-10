read.csv("2015GLMst2.csv",header=T,row.names=1)->d
pca0 <- scale(pca)
result <- prcomp(pca0)
biplot(result,choices=1:2)

names(result)
round(result$rotation,3)
summary(result)

read.csv("2015PCA3.csv",header=T,row.names=1)->pca
pca0 <- scale(pca)
result <- prcomp(pca0)
biplot(result,choices=1:2)



read.csv("2015GLMst2.csv",header=T)->d
cv.2015 <- glm(cv~direction+location+year,family=binomial,data=d)
new.year <- factor(d$year)

model1 <- glm(cbind(cv,100-cv)~1,family=binomial,data=d)
model2 <- glm(cbind(cv,100-cv)~direction,family=binomial,data=d)
model3 <- glm(cbind(cv,100-cv)~location,family=binomial,data=d)
model4 <- glm(cbind(cv,100-cv)~direction+location+year,family=binomial,data=d)
model5 <- glm(cbind(cv,100-cv)~direction*location,family=binomial,data=d)
model6 <- glm(cbind(cv,100-cv)~direction:location,family=binomial,data=d)
model7 <- glm(cbind(cv,100-cv)~direction*location*year,family=binomial,data=d)

model4 <- glm(Cheight~direction+location+year,family=Gamma,data=d)

model4 <- glm(Eflower~direction*location*year,family=poisson,data=d)

install.packages("rpart")
 install.packages("rpart.plot")
 install.packages("partykit")
 library(rpart)
 library(rpart.plot)
 library(partykit)　　　　　　　　#回帰木

ct <- rpart(Ccv ~ direction + location + year, data = d, method = "class")
print(ct)

par(xpd = NA)
plot(ct, branch = 0.8, margin = 0.05)
text(ct, use.n = TRUE, all = TRUE)

rpart.plot(ct, type = 1, uniform = TRUE, extra = 1, under = 1, faclen = 0)

plot(as.party(ct))◎

