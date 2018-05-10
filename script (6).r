###���f�[�^�Z�b�g�쐬
##���f�[�^�V�i���A�y�됅���A�y����AN�ʁAC�ʁA���ώ��x�A���ϋC���j

#�v���b�g��
nc<-101
nr<-101
#���f�[�^�Z�b�g�쐬�A�e�����ɂ��Đ��K���z�ɏ]�������Œl�𐶐�
env<-array(dim = c(nr,nc,7))
for(k in 1 :7){
	env[1,1,k]<-abs(rnorm(1,100,50))
}
for (k in 1 : 7){
for(i in 2 : nc){
	env[1,i,k] <- abs(rnorm(1,100,50))
}
for(j in 2 : nr){
	env[j,1,k] <- abs(rnorm(1,100,50))
}}
#�e�v���b�g�ɂ̒l���A�㉺���E�̃f�[�^�����ƂɌv�Z
for (i in 2 :nc){
for (j in 2 :nr){
for (k in 1 : 7){
		env[j,i,k] <- abs(rnorm(1,1,0.2))*(env[j,i-1,k] + env[j-1,i,k]+3*env[j-1,i-1,k])/5
}}}
for (l in 1 : 10){
for (i in 2 :nc){
for (j in nr-1 :1){
for (k in 1 : 7){
		env[j,i,k] <- runif(n=1,0,1)*(env[j,i-1,k] + env[j+1,i,k])/2
}}}}

#��s�ځA���ڂ�������100*100�̃v���b�g�ɂ���
env2<-array(dim=c(nr-1,nc-1,7))
for (i in 1 : 7){
	env2[,,i]<-env[1:100,1:100,i]
}
#�ʎ�@
env<-array(rep(0,10404),dim=c(102,102,7))
env.rand<-array(dim=c(1000,2,7))
for (k in 1 : 7){
for (i in 1 :1000){
	env.rand[i,,k]<-runif(2,1,102)
}}
for (k in 1 :7){
for (l in 1 :1000){
	j = env.rand[l,1,k]
	i = env.rand[l,2,k]
	env[j,i,k]<- abs(rnorm(1,100,50))
}}
for (l in 1:10){
for (k in 1 :7){
for (i in 2 :101){
for (j in 2 :101){
	e.roundav<-(env[j-1,i-1,k]+env[j-1,i,k]+env[j-1,i+1,k]+env[j,i-1,k]+env[j,i,k]
				+env[j,i+1,k]+env[j+1,i-1,k]+env[j+1,i,k]+env[j+1,i+1,k])/8
	env[j,i,k]<-(2*env[j,i,k]+e.roundav)/3
	
}}}}
#��,�S��s�ځA��,�S���ڂ�������100*100�̃v���b�g�ɂ���
env2<-array(dim=c(nr-1,nc-1,7))
for (i in 1 : 7){
	env2[,,i]<-env[2:101,2:101,i]
}

###�^�̑��ݔ͈́A�ϑ��f�[�^�쐬
#�����T�C�g��
Nsite<-(nc-1)*(nr-1)
#�e�v���b�g�̕��ς̌̐�
int<-rep(1,10000)
##�e�������̐^�̌̐�
#���A�y�됅���AN�ʁA���ϋC����p���Đ��`�A�̐����|�A�\�����z�ɏ]���Đ���
light<-as.vector(env2[,,1])
soilwet<-as.vector(env2[,,2])
nit<-as.vector( env2[,,4])
avetemp<-as.vector( env2[,,7])
Nb<- rpois(Nsite,exp(1/(1+abs(int- 2*light +2*soilwet -nit +avetemp)/100)))
#���W�t���^�̌̐�
N<-array(dim=c(Nsite,3))
j=1
i=1
for (k in 1 : Nsite){
	if (j == 100){
		N[k,1]<-Nb[k]
		N[k,2]<-j
		N[k,3]<-i	
		j=1
		i=i+1
	}else{
		N[k,1]<-Nb[k]
		N[k,2]<-j
		N[k,3]<-i
		j=j+1
}}
colnames(N)<-c("number","row","col")

###�ϑ��l�̍쐬
#�ϑ��l�i�s�݂ɂ͐^�̕s�݁A�U�̕s�݁i�������A�����ł����j�܂ށj
Nob<-array(dim=c(Nsite,3))
for (i in 1 : Nsite ){
#p:�������B�v���b�g�̐^�̌̐��ƁA�v���b�g�̒������s����m���Ɉˑ��B
	p[i] <- atan(((N[i,1]*runif(n=1,0,1))/(max(N[,1])+0.1))*pi/2)  
	Nob[i,1]<-rbinom(1,1,p[i])
	Nob[i,2]<-N[i,2]
	Nob[i,3]<-N[i,3]
}

#�݂̂݃f�[�^�쐬
j<-1
N2<-array(dim=c(Nsite,3))
for (i in 1:length(Nob[,1])){
	if (Nob[i,1]>0){
		N2[j,] <- Nob[i,]
		j=j+1	
}}
N2a<-!is.na(N2[,1])
N2<-N2[1:length(N2a[N2a==TRUE]),]
colnames(N2)<-c("number","row","col")

#�f�[�^�Z�b�g�̐}��
par(mfrow = c(3,3))
for (i in 1 : 7){
image(env[,,i], col=terrain.colors(100))
}
image(matrix(N[,1],ncol=100,nrow=100),col=terrain.colors(5))
plot(N2[,2:3])


#�f�[�^�ϊ�
oc<-Nob[,1]
pred<-data.frame(LIGHT=as.vector(env2[,,1]),SOIL_MOIS=as.vector(env2[,,2]),
	SOIL_DEP=as.vector(env2[,,3]),SOIL_NIT=as.vector(env2[,,4]),SOIL_C=as.vector(env2[,,5]),
	AVE_MOIS=as.vector(env2[,,6]),AVE_TEMP=as.vector(env2[,,7]))

###Maxent
#����
library(dismo)
library(biomod2)
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_92')
library(rJava)
jar <- paste(system.file(package="dismo"), "/java/maxent.jar",sep="")
if (file.exists(jar) & require(rJava)){
 print("OK")
}

#witholding a 20% sample for testing
#fold <- kfold(oc, k=5)
#octest <- oc[fold == 1]
#octrain <- oc[fold != 1]
#fold

#fit model
# use "arges"
	me2<-maxent(pred, oc,args=c("-J","-P"))
me2
# plot showing important of each variable
	plot(me)
#response curves
	response(me)

##�]��
# predict to entire dataset
	r<- predict(me,pred)

ra<-array(r,dim=c(100,100))
oca<-array(oc,dim=c(100,100))
	image(ra,col=terrain.colors(100))
	par(new=T) 
	image(oca,col=c(0:1))

# testing 
#background data
Nc<-array(dim=c(Nsite,3))
for (i in 1:Nsite){
	if (N[i,1]>0){
		Nc[i,1]<-1
		Nc[i,2:3]<-N[i,2:3]
	}else{
		Nc[i,1]<-0
		Nc[i,2:3]<-N[i,2:3]
}}
bg.samp<-sample(nrow(Nc),1000)
bg<-rep(1,10000)
bg<-replace(Nc[,1],bg.samp,0)


#�e�X�g�p
Nc2.temp<-Nc[,1]
Nc2.temp2<-which(Nc2.temp==1)
Nc2.temp2<-Nc2.temp2[sample(length(Nc2.temp2),length(N2)/5)]
Nc2<-rep(0,10000)
Nc2.temp2<-sort(Nc2.temp2)
j=1
for (i in 1 : Nsite){
	if(i == Nc2.temp2[j]&& j != length(Nc2.temp2)){
	Nc2[i]<-1
	j=j+1
	}else if (i == Nc2.temp2[j]){
	Nc2[i]<-1
}}

#simplest way to use "evaluate"

	el<-dismo::evaluate(me,p=Nc2, a=bg, x=pred)
	el
# predict to entire dataset
	r<- predict(me,pred)
	plot(r)
	points(oc)










