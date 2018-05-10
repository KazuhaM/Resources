time 	<- 2
mound <- "CT"
rep   <- 2

if(mound !="CT"){
	flname <- paste(time,"_", mound ,".", rep,sep="")

	a1<-read.csv(paste("p",flname,".csv",sep=""),header=T)
	plot(a1[,5],a1[,2],main=flname,
		xlab="WindSpeed(m/s)",ylab="SandDrifting(n)",col=1,
		xlim = c(0, max(a1[,5][!is.na(a1[,5])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])) )
	par(new=T)
	plot(a1[,5],a1[,3],main="",
		xlab="",ylab="",col=2,
		xlim = c(0, max(a1[,5][!is.na(a1[,5])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])))
	par(new=T)
	plot(a1[,5],a1[,4],main="",
		xlab="",ylab="",col=3,
		xlim = c(0, max(a1[,5][!is.na(a1[,5])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])))	
	legend(locator(1),legend=c("height 10cm","height 20cm","height 40cm"),
		pch=1,col=c(1,2,3))
		dev.copy(pdf, file= paste(flname,".pdf", sep =""))
		dev.off()

} else{
	flname <- paste(time,"_", mound,sep="")

	a1<-read.csv(paste("p",flname,".csv",sep=""),header=T)
	plot(a1[,6],a1[,2],main=flname,
		xlab="WindSpeed(m/s)",ylab="SandDrifting(n)",col=1,
		xlim = c(0, max(a1[,6][!is.na(a1[,6])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])) )
	par(new=T)
	plot(a1[,6],a1[,3],main="",
		xlab="",ylab="",col=2,
		xlim = c(0, max(a1[,6][!is.na(a1[,6])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])))
	par(new=T)
	plot(a1[,6],a1[,4],main="",
		xlab="",ylab="",col=3,
		xlim = c(0, max(a1[,6][!is.na(a1[,6])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])))	
	par(new=T)
	plot(a1[,6],a1[,5],main="",
		xlab="",ylab="",col=4,
		xlim = c(0, max(a1[,6][!is.na(a1[,6])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])))
	legend(locator(1),legend=c("height 10cm","height 20cm","height 40cm","Fixed Dir.(N)"),
		pch=1,col=c(1,2,3,4))
		dev.copy(pdf, file= paste(flname,".pdf", sep =""))
		dev.off()


}