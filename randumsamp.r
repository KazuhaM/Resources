#2016,2017に関してそれぞれランダムサンプリングして点を増やす
library(pscl)
d<-read.csv("16-17erosion5.csv",header=T)
d<-d[!is.na(d$count_5min),]

#2016年データと2017年データを分離
d7<-d[d$year=="2017",]
d6<-d[d$year=="2016",]

k<-10
Nresidual<-k-nrow(d7)%%k
dummyData7<-as.data.frame(matrix(NA,nrow=Nresidual,ncol=ncol(d7)))
names(dummyData7)<-names(d7)
d7.d<-rbind(d7,dummyData7) 
d7.s<-split(d7.d,1:k)
Nresidual<-k-nrow(d6)%%k
dummyData6<-as.data.frame(matrix(NA,nrow=Nresidual,ncol=ncol(d6)))
names(dummyData6)<-names(d6)
d6.d<-rbind(d6,dummyData6) 
d6.s<-split(d6.d,1:k)

minW6<-c(0,0)
minW7<-c(0,0)

for(j in 1 : k){
for (a in 1 : 3){
for (b in 1 : 3){ 
#a=1
#b=1
if(a==3 && b==3){
	next
}else{

#Mound Name
if (a　==　1){
	switch(b,               
	   "1" = mn6<-"13.1",
	   "2" = mn6<-"14.1",
	   "3" = mn6<-"CT"
	)
}else 
if (a == 2){
	switch(b,               
	   "1" = mn6<-"13.2",
	   "2" = mn6<-"14.2",
	   "3" = mn6<-"CT"
	)
}else 
if (a == 3){
	switch(b,               
	   "1" = mn6<-"13.1",
	   "2" = mn6<-"CT"
	)
}
#Slope Direction & File Name & Making data file
if (a==3){
	fln1 <- paste(a, "_", mn6,"N_f",sep="" )
	fln2 <- paste(a, "_", mn6,"SE_f",sep="" )
	fln3 <- paste(a, "_", mn6,"SW_f",sep="" )
	d6.a<-rbind(d6.s[[j]][d6.s[[j]]$try_dune_height==fln1,],d6.s[[j]][d6.s[[j]]$try_dune_height==fln2,],d6.s[[j]][d6.s[[j]]$try_dune_height==fln3 ,])

}else{
	fln1 <- paste(a, "_", mn6,"W_f",sep="" )
	fln2 <- paste(a, "_", mn6,"E_f",sep="" )
	d6.a<-rbind(d6.s[[j]][d6.s[[j]]$try_dune_height==fln1,],d6.s[[j]][d6.s[[j]]$try_dune_height==fln2,])
}

d6.a<-d6.a[order(d6.a[,3]),]
d6.a<-d6.a[!is.na(d6.a$year),]
if(max(d6.a$count_5min)>=1){
minW6<-rbind(minW6,c(min(d6.a$Wind_Speed_5min[d6.a$count_5min >= 1]),d6.a$cover[1]))
}
}}}
minW6<-minW6[2:length(minW6[,1]),]
#names(minW6)<-c("WindSpeed","Coverage")
minW6.lm<-lm(minW6[,1]~minW6[,2])
minW6.p <- predict(minW6.lm)


#2017年データ
for (a in 1 : 3){
for (b in 1 : 6){ 
#a=3
#b=6
if(a<3 && b>=4){
	next
}else{

#Mound Name
if (a　==　1){
	switch(b,               
	   "1" = mn7<-"13.1",
	   "2" = mn7<-"14.1",
	   "3" = mn7<-"CT"
	)
}else if (a == 2){
	switch(b,               
	   "1" = mn7<-"13.2",
	   "2" = mn7<-"14.2",
	   "3" = mn7<-"CT"
	)
}else if (a == 3){
	switch(b,               
	   "1" = mn7<-"13.1",
	   "2" = mn7<-"14.1",
	   "3" = mn7<-"14.2",
	   "4" = mn7<-"13.2",
	   "5" = mn7<-"CT",
	   "6" = mn7<-"SC"

	)
}}
fln1 <- paste(a, "_", mn7,"_10",sep="" )
d7.a<-d7.s[[j]][d7.s[[j]]$try_dune_height==fln1,]
d7.a<-d7.a[order(d7.a[,3]),]
d7.a<-d7.a[!is.na(d7.a$year),]
if(max(d7.a$count_5min)>=1){
minW7<-rbind(minW7,c(min(d7.a$Wind_Speed_5min[d7.a$count_5min >= 1]),d7.a$cover[1]))
}
}}
minW7<-minW7[2:length(minW7[,1]),]
#names(minW7)<-c("WindSpeed","Coverage")
minW7.lm<-lm(minW7[,1]~minW7[,2])
minW7.p <- predict(minW7.lm)
}

#2016、2017両方プロット
windows()
plot(minW6[,2],minW6[,1],xlab="Coverage",ylab="WindSpeed",xlim=c(0,25),ylim=c(0,10),
	col=1,pch=1)
par(new=T)
plot(minW6[,2],minW6.p,type="l",xlim=c(0,25),ylim=c(0,10),col=1,pch=1)
par(new=T)
plot(minW7[,2],minW7[,1],xlab="Coverage",ylab="WindSpeed",xlim=c(0,25),ylim=c(0,10),
	col=2,pch=2)
par(new=T)
plot(minW7[,2],minW7.p,type="l",xlim=c(0,25),ylim=c(0,10),col=2,pch=2)
legend(0,10,c(2016,2017),col=c(1,2),pch=c(1,2))

