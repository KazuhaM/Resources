cv<-read.csv("16-17erosion5.csv",header=T)
d7<-cv[cv$year=="2017",]
vl<-0
nm<-""
c1<-0
for (a in 1 : 2){
for (b in 1 : 3){ 
#a=3
#b=1

#Mound Name
if (a　==　1){
	switch(b,               
	   "1" = mn7<-"13.1",
	   "2" = mn7<-"14.1",
	   "3" = mn7<-"CT"
	)
}else if (a == 2){
	switch(b,               
	   "1" = mn7<-"13.2",
	   "2" = mn7<-"14.2",
	   "3" = mn7<-"CT"
	)
}
fln<-paste("p",a,"_",mn7,sep="")
d<-read.csv(paste(fln,".csv",sep=""),header=T)
d25<-d[,length(d[1,])-3]
d25<-cbind(d25,rep(25,length(d25)))
d10<-d[,length(d[1,])-1]
d10<-cbind(d10,rep(110,length(d10)))
d2<-rbind(d25,d10)
plot(d2[,1],log10(d2[,2]))
d2.lm<-lm(log10(d2[,2])~d2[,1])
summary(d2.lm)
abline(d2.lm)
text(2,4.5,paste("y=",round(d2.lm$coefficients[2],2),"*x + ",
	round(d2.lm$coefficients[1],2),sep=""),adj=0)
mtext(round(d2.lm$coefficients[1],2), side =2, line = 0, at =d2.lm$coefficients[1] )
vl<-c(vl,round(10^d2.lm$coefficients[1],1))
nm<-c(nm,fln)
c1<-c(c1,d7$cover[d7$try_dune_height==paste(a,"_",mn7,"_10",sep="")][1])
	dev.copy(pdf, file=paste(fln,".pdf",sep=""), width = 11, height = 11)
		dev.off()
}}


rslt<-data.frame(vl,nm,c1)
rslt<-rslt[2:length(rslt[,1]),]
plot(c1[2:7],vl[2:7])

