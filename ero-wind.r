#ˆê‰ñ–Ú”ò»Œv
a<-read.csv("a1e.csv",header=T)
a13E <- ts(a$X13E)
a13F <- ts(a$X13F)
aCTE <- ts(a$CTE)
aCTW <- ts(a$CTW)
a14B <- ts(a$X14B)
a14C <- ts(a$X14C)

ts.plot(aCTW,aCTE,a14B,a14C,a13E,a13F,gpars=list(xlab="Time", ylab="”ò»”",lty=c(1:6),col=c(1:6)))
legend(locator(1),c("CTW","CTE","14B","14C","13E","13F"),lty=c(1:6),col=c(1:6))

#ˆê‰ñ–Ú”ò»Œv-•—‘¬Œv
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
ts.plot(aCTW,xlab="time", ylab="”ò»”(ŒÂ)",main ="CTW")
ts.plot(bCTW,xlab="time", ylab="•—‘¬(m/s)")
ts.plot(bCTWd,xlab="time", ylab="•—Œü(degree)")
abline(h = 180 )

dev.off()