time 	<- 3
mound <- "CT"
rep   <- 2

#windows()
#par(mfcol=c(2,3))
if((mound !="CT") && (mound !="SC") ){
	flname <- paste(time,"_", mound ,".", rep,sep="")

	a1<-read.csv(paste("p",flname,".csv",sep=""),header=T)
	plot(a1[,3],a1[,2],main=flname,
		xlab="WindSpeed(m/s)",ylab="SandDrifting(n)",col=1,
		xlim = c(0, max(a1[,3][!is.na(a1[,3])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])) )

} else if(mound !="SC"){
	flname <- paste(time,"_", mound,sep="")

	a1<-read.csv(paste("p",flname,".csv",sep=""),header=T)
	plot(a1[,5],a1[,2],main=flname,
		xlab="WindSpeed(m/s)",ylab="SandDrifting(n)",col=1,
		xlim = c(0, max(a1[,5][!is.na(a1[,5])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])) )
	par(new=T)
	plot(a1[,5],a1[,4],main="",
		xlab="",ylab="",col=2,
		xlim = c(0, max(a1[,5][!is.na(a1[,5])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])))
	par(new=T)
	plot(a1[,5],a1[,3],main="",
		xlab="",ylab="",col=3,
		xlim = c(0, max(a1[,5][!is.na(a1[,5])])),ylim = c(0, max(a1[,2][!is.na(a1[,2])])))	
	legend(locator(1),legend=c("height 10cm","Fixed Dir.(N)","height 20cm"),
		pch=1,col=c(1,2,3))


}else{
	flname <- paste(time,"_", mound,sep="")

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

}

		dev.copy(pdf, file= "Summary.pdf")
		dev.off()