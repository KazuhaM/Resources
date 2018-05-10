par(mfcol = c(5,2))
ts.plot(aCT00,xlab="time", ylab="Wind blow sand(num)",main ="CTTop N   0��")
ts.plot(aCT12,xlab="time", ylab="Wind blow sand(num)",main ="CTTop N 120��")
ts.plot(aCT24,xlab="time", ylab="Wind blow sand(num)",main ="CTTop N 240��")
ts.plot(bCTTl,xlab="time", ylab="Wind speed(m/s)",ylim=c(0,14))
ts.plot(bCTTld,xlab="time", ylab="Wind direction(degree)")
abline(h = 120)
abline(h = 240)

ts.plot(a1300,xlab="time", ylab="Wind blow sand(num)",main ="13-1Top N   0��")
ts.plot(a1312,xlab="time", ylab="Wind blow sand(num)",main ="13-1Top N 120��")
ts.plot(a1324,xlab="time", ylab="Wind blow sand(num)",main ="13-1Top N 240��")
ts.plot(b13T,xlab="time", ylab="Wind speed(m/s)",ylim=c(0,14))
ts.plot(b13Td,xlab="time", ylab="Wind direction(degree)")
abline(h = 120)
abline(h = 240)