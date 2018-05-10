read.csv("data2.csv",header=T,)->data
data
data[,2]
data2<-data[,2]
data3<-data[,3]
data.odds<-data2 / data3
data.odds



read.table("data.txt")-> data2


esoph.alcgp





read.table("data3.txt",header=T,)->data
data
data[,2]
data2<-data[,2]
data3<-data[,3]
data.odds<-data[,1] / data[,2]
data.odds
data.or <- data.odds[-1]/data.odds[1]
data.or

read.table("datan.txt",header=T,)->datan
datan
datan.odds<-datan[,1] / datan[,2]
datan.odds
datan.or <- datan.odds[-1]/datan.odds[1]
datan.or

read.table("datak.txt",header=T,)->datak
datak
datak.odds<-datak[,1] / datak[,2]
datak.odds
datak.or <- datak.odds[-1]/datak.odds[1]
datak.or

read.table("datam.txt",header=T,)->datam
datam
datam.odds<-datam[,1] / datam[,2]
datam.odds
datam.or <- datam.odds[-1]/datam.odds[1]
datam.or
















