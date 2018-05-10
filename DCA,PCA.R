read.csv("DCAŽ{H‹æonly2.csv",header=T)->DCA
result<-decorana(DCA)#DCA
result<-prcomp(DCA)#PCA
plot(result,display="sp")

library(rioja),library(vegan)
summary(result)