#一回目飛砂計
base <- "3_13.2"
pflname<-paste("p" , base ,".csv",sep="")
wflname25<-paste("w", base ,"_25.csv",sep="")


p<-read.csv(pflname,header=T)

p10 <- ts(p[,2])

#一回目飛砂計-風速計
w25<-read.csv(wflname25,header=T)
w25s <- ts(w25[,2])
w25d <- ts(w25[,3])


#グラフ
win.graph(12,11)
par(mfrow = c(3,1))
ts.plot(p10,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_10",sep=""),ylim=c(0,ceiling(max(p10[!is.na(p10)])*1.2)))
ts.plot(w25s,xlab="",ylab= "m/s ",main="Wind speed(m/s) at 25cm height")
abline(h = 4)
ts.plot(w25d,xlab="",ylab= "degree ",main="Wind direction at 25cm height")
abline(h = 120)
abline(h = 240)


		dev.copy(pdf, file=paste(base,".pdf",sep=""), width = 12, height = 11)
		dev.off()

#------------------------------------------------------------------------------------------------
#CT用


#一回目飛砂計
base <- "3_CT"
pflname<-paste("p" , base ,".csv",sep="")
wflname25<-paste("w", base ,"_25.csv",sep="")

p<-read.csv(pflname,header=T)

p10 <- ts(p[,2])
p20 <- ts(p[,3])
pug <- ts(p[,4])

#一回目飛砂計-風速計
w25<-read.csv(wflname25,header=T)
w25s <- ts(w25[,2])
w25d <- ts(w25[,3])

#グラフ
win.graph(12,11)
par(mfrow = c(5,1))
ts.plot(p10,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_10",sep=""),ylim=c(0,ceiling(max(p10[!is.na(p10)])*1.2)))
ts.plot(p20,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_20",sep=""),ylim=c(0,ceiling(max(p20[!is.na(p20)])*1.2)))
ts.plot(pug,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_Fixed Direction(N)",sep=""),ylim=c(0,ceiling(max(pug[!is.na(pug)])*1.2)))
ts.plot(w25s,xlab="",ylab= "m/s ",main="Wind speed(m/s) at 25cm height")
abline(h = 4)
ts.plot(w25d,xlab="",ylab= "degree ",main="Wind direction at 25cm height")
abline(h = 120)
abline(h = 240)

		dev.copy(pdf, file=paste(base,".pdf",sep=""), width = 12, height = 11)
		dev.off()
#-------------------------------------------
#SC用



#一回目飛砂計
base <- "3_SC"
pflname<-paste("p" , base ,".csv",sep="")
wflname25<-paste("w", base ,"_25.csv",sep="")
wflname11<-paste("w", base ,"_110.csv",sep="")

p<-read.csv(pflname,header=T)

p10 <- ts(p[,2])
p20 <- ts(p[,3])
p40 <- ts(p[,4])

#一回目飛砂計-風速計
w25<-read.csv(wflname25,header=T)
w25s <- ts(w25[,2])
w25d <- ts(w25[,3])

w11<-read.csv(wflname11,header=T)
w11s <- ts(w11[,2])
w11d <- ts(w11[,3])

#グラフ
win.graph(12,11)
par(mfrow = c(7,1),pin=c(8,0.3))
ts.plot(p10,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_10",sep=""),ylim=c(0,ceiling(max(p10[!is.na(p10)])*1.2)))
ts.plot(p20,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_20",sep=""),ylim=c(0,ceiling(max(p20[!is.na(p20)])*1.2)))
ts.plot(p40,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_40",sep=""),ylim=c(0,ceiling(max(p40[!is.na(p40)])*1.2)))
ts.plot(w25s,xlab="",ylab= "m/s ",main="Wind speed(m/s) at 25cm height")
abline(h = 4)
ts.plot(w25d,xlab="",ylab= "degree ",main="Wind direction at 25cm height")
abline(h = 120)
abline(h = 240)
ts.plot(w11s,xlab="", ylab= "m/s ",main="Wind speed(m/s) at 110cm height")
abline(h = 4)
ts.plot(w11d,xlab="", ylab= "degree ",main="Wind direction at 110cm height")
abline(h = 120)
abline(h = 240)

		dev.copy(pdf, file=paste(base,".pdf",sep=""), width = 12, height = 11)
		dev.off()
