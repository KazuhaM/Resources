read.table("data3.txt",header=T)->data
data
data.test <- chisq.test(data)
data.test
summary(data.test)

read.table("datan.txt",header=T)->datan
datan
datan.test <- chisq.test(datan)
datan.test
summary(datan.test)

read.table("datak.txt",header=T)->data
datak
datak.test <- chisq.test(datak)
datak.test
summary(datak.test)

read.table("datam.txt",header=T)->datam
data
datam.test <- chisq.test(datam)
datam.test
summary(datam.test)















