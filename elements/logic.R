read.table("data02c.txt")->a
attach(a)
a[,1]
dose_category <- c("2","0","1","3","4","5","6","7","8","9","10","11","12","13","14","15","16")
dose1 <- factor(dose,labels=dose_category)
a.glm<-glm(survive~type+dose1,binomial)
summary(a.glm)