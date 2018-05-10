read.table("relation.txt",header=T)->relation
relation
plot(relation)
relation.dist <- dist(relation,"manhattan")
relation.dist
relation.clust <- hclust(relation.dist,"average")
plot(relation.clust)
plot(relation.clust,main="",sub="",xlab="",ylab="")

library(MASS)
relation.corresp <- corresp(relation)
plot(relation.corresp)
relatiopn.corresp2 <- corresp(relation,nf=2)
plot(relatiopn.corresp2)
plot(relatiopn.corresp2,xlab="1st axis",ylab="2nd axis")


read.table("colon.txt")->colon
names(colon)
library(MASS)
corresp(colon,nf=2) -> colon.corresp
plot(colon.corresp,cex=c(0.7,1),col=c("grey","red"))
colon2<-colon[,"T2"],colon[,"T3"]
plot(colon[,"T2"],colon[,"T37"],xlab="expression in T2”,ylab="expression in T37")



read.table("exercise2.txt",header=T)->exercise2
exercise2
plot(exercise2)
exercise2.dist <- dist(exercise2,"manhattan")
exercise2.dist
exercise2.clust <- hclust(exercise2.dist,"average")
plot(exercise2.clust)
plot(exercise2.clust,main="",sub="",xlab="",ylab="")

library(MASS)
exercise2.corresp <- corresp(exercise2)
plot(exercise2.corresp)
exercise2.corresp2 <- corresp(exercise2,nf=2)
plot(exercise2.corresp2)
plot(exercise2.corresp2,xlab="1st axis",ylab="2nd axis")

