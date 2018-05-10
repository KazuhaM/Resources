#一回目飛砂計
for(i in 1 : 3){
if(i ==3){
	m<-6
}else{
	m<-3
}
for(j in 1 : m){
if(i ==2){
	j <- j+2
}

mnd<-c("13.1","14.1","CT","13.2","14.2","SC")

base <- paste(i,"_",mnd[j],sep="")
pflname<-paste("p" , base ,".csv",sep="")
p<-read.csv(pflname,header=T)

if(i==1 || i ==2){
if(j==1 || j==2 || j==3 || j==4){

p10 <- ts(p[,2])
p20 <- ts(p[,3])
p40 <- ts(p[,4])

#一回目飛砂計-風速計
w25s <- ts(p[,5])
w25d <- ts(p[,6])

w11s <- ts(p[,7])
w11d <- ts(p[,8])

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

#------------------------------------------------------------------------------------------------
#CT用


#一回目飛砂計
base <- "2_CT"
pflname<-paste("p" , base ,".csv",sep="")
wflname25<-paste("w", base ,"_25.csv",sep="")
wflname11<-paste("w", base ,"_110.csv",sep="")

p<-read.csv(pflname,header=T)

p10 <- ts(p[,2])
p20 <- ts(p[,3])
p40 <- ts(p[,4])
pug <- ts(p[,5])

#一回目飛砂計-風速計
w25<-read.csv(wflname25,header=T)
w25s <- ts(w25[,2])
w25d <- ts(w25[,3])

w11<-read.csv(wflname11,header=T)
w11s <- ts(w11[,2])
w11d <- ts(w11[,3])

#グラフ
win.graph(12,11)
par(mfrow = c(8,1),pin=c(8,0.3))
ts.plot(p10,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_10",sep=""),ylim=c(0,ceiling(max(p10[!is.na(p10)])*1.2)))
ts.plot(p20,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_20",sep=""),ylim=c(0,ceiling(max(p20[!is.na(p20)])*1.2)))
ts.plot(p40,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_40",sep=""),ylim=c(0,ceiling(max(p40[!is.na(p40)])*1.2)))
ts.plot(pug,xlab="", ylab="(num)",main =paste("Drifting Sand " ,base , "_Fixed Direction(N)",sep=""),ylim=c(0,ceiling(max(pug[!is.na(pug)])*1.2)))
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

