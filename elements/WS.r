#install.packages("rgl", dependencies = TRUE)
#library(rgl)
#x<-read.csv("WS.csv",header=T)
#plot3d(x[,1],x[,2],x[,3])
for(i in 1 : 12){
a<-i
switch(a,
	"1"=flname<-"13.1W",
	"2"=flname<-"13.1E",	
	"3"=flname<-"13.2W",
	"4"=flname<-"13.2E",
	"5"=flname<-"14.1W",
	"6"=flname<-"14.1E",
	"7"=flname<-"14.2W",
	"8"=flname<-"14.2E",
	"9"=flname<-"CTW",
	"10"=flname<-"CTE",
	"11"=flname<-"13.1Top",
	"12"=flname<-"CTTop"
)

a1<-read.csv(paste(flname,".csv",sep=""),header=T)
plot(a1[,3],a1[,5],main=flname,sub=paste(a1[1,6],"%",sep=""),
	xlab="WindSpeed(m/s)",ylab="SandDrifting(n)")
	dev.copy(pdf, file= paste(flname,".pdf", sep =""))
	dev.off()

}
