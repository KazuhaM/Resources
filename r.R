read.table("‚†‚“‚‚.txt")->f
names(f)
library(MASS)
corresp(f) -> f.corresp
plot(f.corresp,cex=c(0.7,1),col=c("grey","red"))


read.table("‚†‚“‚‚3.txt")->f3
names(f3)
library(MASS)
corresp(f3,nf=2) -> f3.corresp
plot(f3.corresp,cex=c(0.7,1),col=c("grey","red"))

read.table("‚†‚“‚‚4.txt")->f4
names(f4)
library(MASS)
corresp(f4) -> f4.corresp
plot(f4.corresp,cex=c(0.7,1),col=c("grey","red"))


