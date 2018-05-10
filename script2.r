###���f�[�^�Z�b�g�쐬
##���f�[�^�V�i���A�y�됅���A�y����AN�ʁAC�ʁA���ώ��x�A���ϋC���j

#�v���b�g��
nc<-101
nr<-101
#�����������ʒu��
e.r<-c(1000,100,30,30,30,15,15)
#���f�[�^�킲�Ƃ̂ڂ����
b<-c(5,10,20,20,20,100,100)
#���f�[�^�������l��
e.w<-c(10000,10000,10000,10000,10000,10000,10000)
#���f�[�^�Z�b�g�쐬�A�e�����ɂ��Đ��K���z�ɏ]�������Œl�𐶐�
env<-array(rep(10,(nr+1)*(nc+1)),dim=c(nr+1,nc+1,7))
#�����_���ȃv���b�g�ɋɑ�l�𐶐�
env.rand<-array(dim=c(max(e.r),2,7))
for (k in 1 : 7){
for (i in 1 :e.r[k]){
	env.rand[i,,k]<-runif(2,1,nr+1)
}}
for (k in 1 :7){
for (l in 1 :e.r[k]){
	j = env.rand[l,1,k]
	i = env.rand[l,2,k]
	env[j,i,k]<- abs(rnorm(1,e.w[k],e.w[k]/3))
}}
#�ɑ�l�����ӂɂڂ���
for (k in 1 :7){
for (l in 1 :b[k]){
for (i in 2 :nc){
for (j in 2 :nr){
	e.roundav<-(env[j-1,i-1,k]+env[j-1,i,k]+env[j-1,i+1,k]+env[j,i-1,k]+env[j,i,k]
				+env[j,i+1,k]+env[j+1,i-1,k]+env[j+1,i,k]+env[j+1,i+1,k])/8
	env[j,i,k]<-(env[j,i,k]+e.roundav)/2
	
}}}}
#��,�S��s�ځA��,�S���ڂ�������100*100�̃v���b�g�ɂ���
env2<-array(dim=c(nr-1,nc-1,7))
for (i in 1 : 7){
	env2[,,i]<-env[2:101,2:101,i]
}
env.name<-c("Light","SOIL_MOIS","SOIL_DEP","SOIL_NIT","SOIT_CAR","AVE_MOIS","AVE_TWMP")

###�^�̑��ݔ͈́A�ϑ��f�[�^�쐬
#�����T�C�g��
Nsite<-(nc-1)*(nr-1)
#�e�v���b�g�̕��ς̌̐�
int<-rep(1,(nc-1)*(nr-1))
##�e�������̐^�̌̐�
#�I�[�_�[����
light<-as.vector(env2[,,1])/10
soilmois<-as.vector(env2[,,2])/2
soildep<-as.vector(env2[,,3])
soilnit<-as.vector( env2[,,4])
soilc<-as.vector(env2[,,5])
avemois<-as.vector(env2[,,6])/300
avetemp<-as.vector( env2[,,7])/300
#���A�y�됅���AN�ʁA���ϋC����p���Đ��`�����A�̐����|�A�\�����z�ɏ]���Đ���
Nb<- rpois(Nsite,2*exp(1/(1+abs(int- 2*light +2*soilmois -soilnit�@+soilc +avetemp)/100)))
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
p<-c(NULL)
for (i in 1 : Nsite ){
#p:�������B�v���b�g�̐^�̌̐��ƁA�v���b�g�̒������s����m���Ɉˑ��B
	p[i] <- (1+3/-(N[i,1]+3))*runif(n=1,0,1)
	Nob[i,1]<-rbinom(1,1,p[i])
	Nob[i,2]<-N[i,2]
	Nob[i,3]<-N[i,3]
}

#�ϑ��l�݂̍̂݃f�[�^�쐬
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
image(env[,,i], col=terrain.colors(100),main=env.name[i])
}
image(matrix(N[,1],ncol=100,nrow=100),col=terrain.colors(5),main="TRUE_NUNBER_OF_SPECIES")
plot(N2[,2:3],main="PLOT_OBSERVING_SPECIES")


###Maxent

#�f�[�^�ϊ�
oc<-Nob[,1]
pred<-data.frame(LIGHT=as.vector(env2[,,1]),SOIL_MOIS=as.vector(env2[,,2]),
	SOIL_DEP=as.vector(env2[,,3]),SOIL_NIT=as.vector(env2[,,4]),SOIL_C=as.vector(env2[,,5]),
	AVE_MOIS=as.vector(env2[,,6]),AVE_TEMP=as.vector(env2[,,7]))
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
	plot(me2)
#response curves
	response(me2)

##�]��
# predict to entire dataset
	pred.result<- predict(me2,pred)

ra<-array(pred.result,dim=c(100,100))
	image(ra,col=terrain.colors(100))
	par(new=T) 
	plot(N2[,2:3],main="observe/predict")

#���f-�^�Z�b�g���烉���_���ɒ��o
pred.samp <- sample(nrow(pred),1000)
pred.bg <- pred[sort(pred.samp),]

#�ϑ����m�F���ꂽ�v���b�g����e�X�g�p�ɒ��o
N2.samp <- sample(nrow(N2),100)
N2.plot <-N2[N2.samp,2:3]
N2.test<-array(dim=c(nrow(N2.plot),7))
pl.num<-c(NULL)
for (k in 1:nrow(N2.plot)){
	j<-N2.plot[k,1]
	i<-N2.plot[k,2]
	pl.num[k]<-100*(i-1)+j
}
N2.test<-pred[sort(pl.num),]

#evaluate
	e2<-dismo::evaluate(me2,p=N2.test,a=pred.bg)
e2

