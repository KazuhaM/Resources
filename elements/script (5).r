#一回目飛砂計
a<-read.csv("a1e.csv",header=T)
a13E <- ts(a$X13E)
a13F <- ts(a$X13F)
aCTE <- ts(a$CTE)
aCTW <- ts(a$CTW)
a14B <- ts(a$X14B)
a14C <- ts(a$X14C)

ts.plot(aCTE,aCTW,a14C,a14B,a13F,a13E,gpars=list(xlab="Time", ylab="Drifting Sand",lty=c(1:6),col=c(1:6),
	main="Time series variations of Drifting Sand",sub="Date of investigation: 2016.8.2-8.5"))
legend(locator(1),c("CTE","CTW","14-1E","14-1W","13-1E","13-1W"),lty=c(1:6),col=c(1:6))

#一回目飛砂計-風速計
b<-read.csv("a1w.csv",header=T)
b13E <- ts(b$X13EWS)
b13F <- ts(b$X13FWS)
bCTE <- ts(b$CTEWS)
bCTW <- ts(b$CTWWS)
b14B <- ts(b$X14BWS)
b14C <- ts(b$X14CWS)

b13Ed <- ts(b$X13ETD)
b13Fd <- ts(b$X13FTD)
bCTEd <- ts(b$CTETD)
bCTWd <- ts(b$CTWTD)
b14Bd <- ts(b$X14BTD)
b14Cd <- ts(b$X14CTD)


par(mfrow = c(3,1))
ts.plot(aCTW,xlab="time", ylab="飛砂数(個)",main ="CTW")
ts.plot(bCTW,xlab="time", ylab="風速(m/s)")
ts.plot(bCTWd,xlab="time", ylab="風向(degree)")
abline(h = 180 )

dev.off()