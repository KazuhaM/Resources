read.table("exam12.txt")->exam
round(exam,3)
plot(y~x,data=subset(exam,sex=="0"),
xlab="age",ylab="maturity",
xlim=c(0,17),ylim=c(0,1),col="blue")
x.glm <-
glm(y~x+sex,binomial,exam)
summary(x.glm)








