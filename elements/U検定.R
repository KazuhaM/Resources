install.packages("exactRankTests", repos="http://cran.ism.ac.jp/")
library(exactRankTests)

a<-2

switch(a,
	"1" = flname <- "Elymus2",
	"2" = flname <- "Caragana2"
)
if (a==1){
	yl<-"Cover Elymus spp."
}else 
if (a == 2){
	yl<-"Cover C. microphylla"
}

x<- read.csv(paste(flname,".csv",sep=""),header=TRUE)
d<-x[,1]
e<-x[,2]
wilcox.exact(d,e,paired=F)

enn<-ncol(x)
x.mean<-apply(x[1:enn-1], 2 , mean , na.rm=T)
x.se<-c()
hm<-c()
sda<-c()
hma<-c()
enna<-enn-1
for (i in 1 : enna){
	#if(is.na(names(table(is.na(x[,i])==FALSE)["TRUE"]))){
	#	hm[i]<-0
	#	break
	#	}else
		print(i)
		if(table(is.na(x[,i])==FALSE)["TRUE"]==length(x[,i])){
			hm[i]<-length(x[,i])
		}else {
			hm[i]<-table(is.na(x[,i])==FALSE)["TRUE"]
		}
	sda[i]<-sd(x[,i], na.rm=T)
	hma[i]<-sqrt(hm[i])
	x.se[i]<-sda [i]/ hma[i]
}

x.sd <- apply(x[1:enn-1], 2, sd, na.rm = T)

#棒グラフの記述
xh<-max(x.mean + x.se)*ms
px <- barplot(x.mean, ylab = yl, xlab = xl, ylim = c(0,xh+mxs), ps = ft)
#誤差バーを描く、lengthは誤差バーのバーの長さを示す、angleを鋭角にするとバーは矢印状となる
arrows(px, x.mean - x.se, px, x.mean + x.se, angle = 90, length = 0.1)
arrows(px, x.mean + x.se, px, x.mean - x.se, angle = 90, length = 0.1)

		dev.copy(pdf, file=paste(flname,"a.pdf",sep=""), width = 10, height = 10)
		dev.off()














