summary(d)
sd(d)

### �������ݒ�@�C�ӂ̐��ɕύX���Ă��������@###
gnum_st<-5 ##�n�_
gnum_sp<-5�@##��


### �f�[�^�ǂݍ��݁@###
library(vegan) 
library(labdsv) #indval�p
d<-read.csv("WE_Veg_over3times2.csv",row.names=1)
#d[is.na(d)]<-0
d<-t(d)
#d<-t(d)
col<-ncol(d)
row<-nrow(d)
is.numeric(d)
d

result<-list()
r<-1
# DCA�̏ꍇ
#d.mds<-decorana(d)
#MDS�̏ꍇ
d.mds<-metaMDS(d,zerodist="add") ##### zerodist="add" ��ǉ�
d.mds
summary(d.mds)
stressplot(d.mds)
#PCA�̏ꍇ
#d.mds<-prcomp(d)
#d.mds
#plot(d.mds)
#summary(d.mds)
#biplot(d.mds,choices=1:2)
#biplot(d.mds)

##### �N���X�^�[���� ####
sco<-d.mds$points�@#���񉻂��ꂽ���W�l
result[[r]]<-sco
r=r+1
km <- cascadeKM(sco, 2, 6) #kmeans�@�ɂ���K�w�N���X�^���� 2�`6����������
plot(km) #�����̌��ʂ��m�F�@Calinski-Harabasz �̊
#���̌��ʂōœK�ȕ����������聫�ɔ��f
dnum<-5 #�ϐ��ɕ���������
memb<-km$partition[,dnum-1] #�O���[�v���ǂ̂悤�ɕ������ꂽ�����擾 
result[[r]]<-memb
r=r+1

##### �w�W�핪�� #####
dul<-indval(d,memb) #labdsv�𗘗p
result[[r]]<-dul$pval
r=r+1
duli<-dul$indval�@#indval�̒l
result[[r]]<-duli
r=r+1
kms<-c(1:dnum)
for( i in n<-seq(length(duli))) n[i]<-rownames(duli)[duli[,i]==max(duli[,i])] #�n�_�̍ō��w�W��
#�w�W�팋�ʂ̐��`�@�Q�l�@http://kobe.cool.ne.jp/matsut/r38.txt
dul<-data.frame(numeric.sp=names(dul$maxcls), community=dul$maxcls, indval=round(dul$indcls,3), pvalue=round(dul$pval,4))
dul<- dul[order(dul$community, dul$indval, dul$pvalue, decreasing=T),][,-1]
result[[r]]<-dul
r=r+1

##### ��} #####
ordipointlabel(d.mds,display="sites",col = (kms+1)[memb],pch = 1,cex=1)
ordiellipse(d.mds,memb, display="sites",kind="sd", conf=0.9,lwd=1,lty=2, col="black")
x<-tapply(sco[,1],memb,mean) #�O���[�v�̏d�S
y<-tapply(sco[,2],memb,mean) 
par(new=T)
text(x,y,n,col= c(kms+1))�@#�O���[�v���ƂɎw�W���\��
dul #�w�W��̈ꗗ���m�F

#memb.o<-c()
#for(i in 1 : length(memb)){
#	temp<-memb[names(memb)==names(x.mean)[i]]
#	names(temp)<-NULL
#	memb.o<-c(memb.o,temp)
#}
#sum<-data.frame(x.mean,x.se,memb.o)

library(pscl)
e<-read.csv("16-17erosion5.csv",header=T)
e<-e[!is.na(e$count_5min),]

#2016�N�f�[�^��2017�N�f�[�^�𕪗�
d7<-e[e$year=="2017",]
d6<-e[e$year=="2016",]


#2017�N�f�[�^
minW7<-data.frame()
for (a in 1 : 3){
for (b in 1 : 6){ 
#a=3
#b=6
if(a<3 && b>=4){
	next
}else{

#Mound Name
if (a�@==�@1){
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
	minW7<-rbind(minW7,
	data.frame("WindSpeed"=min(d7.a$Wind_Speed_5min[d7.a$count_5min >= 1]),
	"Coverage"=d7.a$cover[1],
	"DuneName"=paste("X",d7.a$dune[1],sep="")))
	#minW7.name<-c(minW7.name,mn7)
}
}}


#2016�N���
minW6<-data.frame()
for (a in 1 : 3){
for (b in 1 : 3){ 
#a=1
#b=1
if(a==3 && b==3){
	next
}else{

#Mound Name
if (a�@==�@1){
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
	minW6<-rbind(minW6,
	data.frame("WindSpeed"=min(d6.a$Wind_Speed_5min[d6.a$count_5min >= 1]),
	"Coverage"=d6.a$cover[1],
	"DuneName"=paste("X",d6.a$dune[1],sep="")))
	#minW7.name<-c(minW6.name,mn6)
}
}}}
minW<-rbind(minW6,minW7)

#2016�A2017�����v���b�g
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

minW.g<-data.frame()
for(i in 1:nrow(minW)){
	minW.g<-rbind(minW.g,data.frame(minW[i,],"Group"=memb[names(memb)==minW$DuneName[i]]))
}
minW.g<-minW.g[order(minW.g$Group),]
minW.g
result[[r]]<-minW.g
r<-r+1

minW.lm<-list()
for(i in 1:5){
	minW.lm[[i]]<-lm(minW.g$WindSpeed[minW.g$Group==i]~minW.g$WindSpeed[minW.g$Group==i])
}
windows()
for(i in 1 : 5){
	if(i>1){
	par(new=T)
	}
	plot(minW.g$Coverage[minW.g$Group==i],minW.g$WindSpeed[minW.g$Group==i],col=i,pch=i,
	xlim=c(0,21),ylim=c(0,9),ylab="Threshold Wind Speed(m/s)",xlab="Coverage (%)",
	cex.lab=1.3)
}
legend(0,8,c(1,2,3,4,5),col=c(1,2,3,4,5),pch=c(1,2,3,4,5))