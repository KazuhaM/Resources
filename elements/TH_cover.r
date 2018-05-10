library(pscl)
d<-read.csv("16-17erosion5.csv",header=T)
d<-d[!is.na(d$count_5min),]

#2016年データと2017年データを分離
d7<-d[d$year=="2017",]
d6<-d[d$year=="2016",]


#2017年データ
minW7<-c(0,0)
minW7.name<-"test"
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
d7.a<-d7[d7$try_dune_height==fln1,]
d7.a<-d7.a[order(d7.a[,3]),]
if(max(d7.a$count_5min)>=1){
minW7<-rbind(minW7,c(min(d7.a$Wind_Speed_5min[d7.a$count_5min >= 1]),d7.a$cover[1]))
minW7.name<-c(minW7.name,mn7)
}
}}
minW7<-minW7[2:length(minW7[,1]),]
#names(minW7)<-c("WindSpeed","Coverage")
minW7.name<-minW7.name[2:length(minW7.name)]
minW7.lm<-lm(minW7[,1]~minW7[,2])
minW7.p <- predict(minW7.lm)
plot(minW7[,2],minW7[,1],xlim=c(0,25),ylim=c(0,10),
	xlab="Coverage(%)",ylab="Threshold Wind Speed (m/s)")
par(new=T)
plot(minW7[,2],minW7.p,type="l",xlim=c(0,25),ylim=c(0,10),
	xlab="",ylab="")

#2016年解析
minW6<-c(0,0)
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
	d6.a<-rbind(d6[d6$try_dune_height==fln1,],d6[d6$try_dune_height==fln2,],d6[d6$try_dune_height==fln3 ,])

}else{
	fln1 <- paste(a, "_", mn6,"W_f",sep="" )
	fln2 <- paste(a, "_", mn6,"E_f",sep="" )
	d6.a<-rbind(d6[d6$try_dune_height==fln1,],d6[d6$try_dune_height==fln2,])
}
d6.a<-d6.a[order(d6.a[,3]),]
if(max(d6.a$count_5min)>=1){
minW6<-rbind(minW6,c(min(d6.a$Wind_Speed_5min[d6.a$count_5min >= 1]),d6.a$cover[1]))
}
}}}
minW6<-minW6[2:length(minW6[,1]),]
#names(minW6)<-c("WindSpeed","Coverage")
minW6.lm<-lm(minW6[,1]~minW6[,2])
minW6.p <- predict(minW6.lm)
plot(minW6[,2],minW6[,1],xlab="Coverage(%)",ylab="Threshold Wind Speed (m/s)",
	xlim=c(0,25),ylim=c(0,10))
par(new=T)
plot(minW6[,2],minW6.p,type="l",xlim=c(0,25),ylim=c(0,10),xlab="",ylab="")

#2016、2017両方プロット
windows()
plot(minW6[,2],minW6[,1],xlab="Coverage(%)",ylab="Threshold Wind Speed (m/s)",xlim=c(0,25),ylim=c(0,10),
	col=1,pch=1)
par(new=T)
plot(minW6[,2],minW6.p,type="l",xlim=c(0,25),ylim=c(0,10),col=1,pch=1,xlab="",ylab="")
par(new=T)
plot(minW7[,2],minW7[,1],xlab="",ylab="",xlim=c(0,25),ylim=c(0,10),
	col=2,pch=2)
par(new=T)
plot(minW7[,2],minW7.p,type="l",xlim=c(0,25),ylim=c(0,10),col=2,pch=2,xlab="",ylab="")
legend(0,10,c(2016,2017),col=c(1,2),pch=c(1,2))

#土壌水分を入れて重回帰？GLM？
minW67<-rbind(minW6,minW7)
minW67
hist(minW67[,1])
shapiro.test(x=minW67[,1])

hist(minW7[,1])
shapiro.test(x=minW7[,1])
shapiro.test(x=minW6[,1])

SM<-read.csv("17M-t.csv",header=T)
#各砂丘の平均土壌水分量
SM.a<-c()
for(i in 1:6){
	SM.a[i]<-mean(SM[!is.na(SM[,i]),i])
}
names(SM.a)<-names(SM)
n.t<-0
for(i in 1:length(minW7.name))
switch(minW7.name[i],
	"CT"= n.t<-c(n.t,1),
	"SC"= n.t<-c(n.t,2),
	"14.1"= n.t<-c(n.t,3),
	"14.2"= n.t<-c(n.t,4),
	"13.1"= n.t<-c(n.t,5),
	"13.2"= n.t<-c(n.t,6)
)
n.t<-n.t[2:length(n.t)]
SM.a.o<-0
for(i in 1:length(n.t)){
	SM.a.o<-c(SM.a.o,SM.a[n.t[i]])
}
SM.a.o<-SM.a.o[2:length(SM.a.o)]
minW7.m<-cbind(minW7,SM.a.o)
colnames(minW7.m)<-c("ThresholdWS","Cover","SoilMoisture")
minW7.m<-data.frame(minW7.m)
result<-lm(ThresholdWS~Cover*SoilMoisture,data=minW7.m)
summary(result)
result2<-lm(ThresholdWS~Cover,data=minW7.m)
result3<-lm(ThresholdWS~SoilMoisture,data=minW7.m)
summary(result2)
summary(result3)
plot(minW7.m$Cover,minW7.m$ThresholdWS,xlab="Coverage (%)",
	ylab="Threshold Wind Speed (m/s)",xlim=c(0,22),ylim=c(0,6))
abline(result2)
windows()
plot(minW7.m$SoilMoisture,minW7.m$ThresholdWS,xlab="SoilMoisture (Vol%)",
	ylab="Threshold Wind Speed (m/s)")
abline(result3)


