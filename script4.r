###���f�[�^�Z�b�g�쐬
##���f�[�^�V�i���A�y�됅���A�y����AN�ʁAC�ʁA���ώ��x�A���ϋC���j
#�v���b�g��
nc<-101
nr<-101

#�����������N�_��
e.r<-c(50,40,50,40,40,15,15)
#���f�[�^�킲�Ƃ̂ڂ����
b<-c(30,150,30,150,150,200,100)
#���f�[�^�������l��
e.w<-c(10000,10000,10000,10000,10000,10000,10000)

#���f�[�^�Z�b�g�쐬�A�e�����ɂ��Đ��K���z�ɏ]�������Œl�𐶐�
env<-array(rep(10,(nr+1)*(nc+1)),dim=c(nr+1,nc+1,7))
#�����_���ȃv���b�g�ɋɑ�l�i�N�_�j�𐶐�
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
#�N�_�̒l�����ӂɂڂ���
for (k in 1 :7){
for (l in 1 :b[k]){
for (i in 2 :nc){
for (j in 2 :nr){
	e.roundav<-(env[j-1,i-1,k]+env[j-1,i,k]+env[j-1,i+1,k]+env[j,i-1,k]+env[j,i,k]
				+env[j,i+1,k]+env[j+1,i-1,k]+env[j+1,i,k]+env[j+1,i+1,k])/8
	env[j,i,k]<-(env[j,i,k]+e.roundav)/2
	
}}}}
#1�A101�s�ځA1�A101�ڂ�������100*100�̃v���b�g�ɂ���
env2<-array(dim=c(nr-1,nc-1,7))
for (i in 1 : 7){
	env2[,,i]<-env[2:101,2:101,i]
}
env.name<-c("Light","SOIL_MOIS","SOIL_DEP","SOIL_NIT","SOIT_CAR","AVE_MOIS","AVE_TWMP")


###�^�̑��ݔ͈́A�ϑ��f�[�^�쐬
#�����T�C�g��
Nsite<-(nc-1)*(nr-1)
#�e�v���b�g�̕��ς̌̐�
int<-rep(1.5,(nc-1)*(nr-1))
##�e�������̐^�̌̐�
#�I�[�_�[����
light<-as.vector(env2[,,1])/10
soilmois<-as.vector(env2[,,2])/2
soildep<-as.vector(env2[,,3])
soilnit<-as.vector( env2[,,4])
soilc<-as.vector(env2[,,5])
avemois<-as.vector(env2[,,6])/300
avetemp<-as.vector( env2[,,7])/300
#���A�y�됅���AN�ʁAC�ʁA���ϋC������`�����A�̐����덷���z�|�A�\�����z�ɏ]���Đ���
Nb<- rpois(Nsite,int- 2*light/max(light) +2*soilmois/max(soilmois) 
		-soilnit/max(soilnit)�@+soilc/max(soilc) +avetemp/max(avetemp))

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

#�^�̍ݕs�݃f�[�^
N.ex<-c(NULL)
for (i in 1:length(N[,1])){
	if (N[i,1]==0){
		N.ex[i]<-0
	}else{
		N.ex[i]<-1
}}

###�ϑ��l�̍쐬
#�ϑ��l�i�s�݂ɂ͐^�̕s�݁A�U�̕s�݁i�������A�����ł����j�܂ށj
Nob<-array(dim=c(Nsite,3))
p<-c(NULL)
for (i in 1 : Nsite ){
#p:�������B�v���b�g�̐^�̌̐��ƁA�v���b�g�̒������s����m���Ɉˑ��B
	p[i] <- (1+4/-(N[i,1]+4))**abs(1-rpois(n=1,lambda=2)/10)
	Nob[i,1]<-rbinom(1,1,p[i])
	Nob[i,2]<-N[i,2]
	Nob[i,3]<-N[i,3]
}

#�ϑ��l�݂̍̂݃f�[�^�쐬
j<-1
Nob.ex<-array(dim=c(Nsite,3))
for (i in 1:length(Nob[,1])){
	if (Nob[i,1]>0){
		Nob.ex[j,] <- Nob[i,]
		j=j+1	
}}
Nob.exa<-!is.na(Nob.ex[,1])
Nob.ex<-Nob.ex[1:length(Nob.exa[Nob.exa==TRUE]),]
colnames(Nob.ex)<-c("number","row","col")

#�f�[�^�Z�b�g�̐}��
library(fields)
par(mfrow = c(3,3))
for (i in 1 : 7){
image.plot(env[,,i], col=terrain.colors(100),main=env.name[i])
}
image.plot(matrix(N[,1],ncol=100,nrow=100),col=terrain.colors(10),main="TRUE_NUNBER_OF_SPECIES")
plot(Nob.ex[,2:3],main="PLOT_OBSERVING_SPECIES")

#�f�[�^�ϊ�
oc<-Nob[,1]
pred<-data.frame(LIGHT=as.vector(env2[,,1]),SOIL_MOIS=as.vector(env2[,,2]),
	SOIL_DEP=as.vector(env2[,,3]),SOIL_NIT=as.vector(env2[,,4]),SOIL_C=as.vector(env2[,,5]),
	AVE_MOIS=as.vector(env2[,,6]),AVE_TEMP=as.vector(env2[,,7]))


###GLM
##�݂̂݊ϑ��f�[�^��GLM
pred.glm<-cbind(Nob[,1],pred)
colnames(pred.glm)<-c("observed",colnames(pred.glm[,2:8]))
#GLM
result.glm <- glm(observed ~. , family = binomial, data = pred.glm)
result.glm
#stepAIC
library(MASS)
result.stepAIC <- stepAIC(result.glm)
summary(result.glm)
summary(result.stepAIC)

##�^�̌̐��f�[�^��GLM
pred.glmtv<-cbind(N[,1],pred)
colnames(pred.glmtv)<-c("true_value",colnames(pred.glmtv[,2:8]))
#GLM
result.glmtv <- glm(true_value ~. , family = poisson, data = pred.glmtv)
result.glmtv
#stepAIC
library(MASS)
result.stepAICtv <- stepAIC(result.glmtv)
summary(result.glmtv)
summary(result.stepAICtv)


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

#fit model
# use "arges"
	me2<-maxent(pred, oc,args=c("-J","-P"))
me2
# plot showing important of each variable
	plot(me2)
#response curves
	response(me2)

##����
# predict to entire dataset
	pred.result<- predict(me2,pred)
#���茋�ʂ̐}��
ra<-array(pred.result,dim=c(100,100))
	image.plot(ra,col=terrain.colors(100))
	par(new=T) 
	plot(Nob.ex[,2:3],main="observe/predict")

##�]��
#���f-�^�Z�b�g����back ground�������_���ɒ��o
pred.samp <- sample(nrow(pred),1000)
pred.bg <- pred[sort(pred.samp),]

#�ϑ����m�F���ꂽ�v���b�g����e�X�g�p�ɒ��o
Nob.ex.samp <- sample(nrow(Nob.ex),100)
Nob.ex.plot <-Nob.ex[Nob.ex.samp,2:3]
Nob.ex.test<-array(dim=c(nrow(Nob.ex.plot),7))
pl.num<-c(NULL)
for (k in 1:nrow(Nob.ex.plot)){
	j<-Nob.ex.plot[k,1]
	i<-Nob.ex.plot[k,2]
	pl.num[k]<-100*(i-1)+j
}
Nob.ex.test<-pred[sort(pl.num),]

#evaluate
	e2<-dismo::evaluate(me2,p=Nob.ex.test,a=pred.bg)
e2

