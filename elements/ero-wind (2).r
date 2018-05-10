#一回目飛砂計
a<-read.csv("a3e13.csv",header=T)
c<-read.csv("a3eCT.csv",header=T)
a1300 <- ts(a$X13T0)
a1312 <- ts(a$X13T120)
a1324 <- ts(a$X13T240)
aCT00 <- ts(c$CTT0)
aCT12 <- ts(c$CTT120)
aCT24 <- ts(c$CTT240)

#一回目飛砂計-風速計
b<-read.csv("a3w.csv",header=T)
b13T <- ts(b$X13TWS)
bCTTl <- ts(b$CTTlWS)
bCTTh <- ts(b$CTThWS)

b13Td <- ts(b$X13TTD)
bCTTld <- ts(b$CTTlTD)
bCTThd <- ts(b$CTThTD)

