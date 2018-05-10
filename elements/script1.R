read.table("e.txt",header=T)->e
e
plot(e)
e.dist <- dist(e,"manhattan")
e.dist
e.clust <- hclust(e.dist,"average")
plot(e.clust,main="",sub="",xlab="",ylab="")
library(MASS)
e.corresp <- corresp(e)
plot(e.corresp)
e.corresp2 <- corresp(e,nf=2)
plot(corresp(e,nf=2),cex=c(1,0.7),col=c("grey","red"),xlab="1st axis",ylab="2nd axis")

































