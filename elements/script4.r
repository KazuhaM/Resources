###環境データセット作成
##環境データ７つ（光、土壌水分、土壌厚、N量、C量、平均湿度、平均気温）
#プロット数
nc<-101
nr<-101

#環境乱数発生起点数
e.r<-c(50,40,50,40,40,15,15)
#環境データ種ごとのぼかし具合
b<-c(30,150,30,150,150,200,100)
#環境データ乱数数値幅
e.w<-c(10000,10000,10000,10000,10000,10000,10000)

#環境データセット作成、各条件について正規分布に従う乱数で値を生成
env<-array(rep(10,(nr+1)*(nc+1)),dim=c(nr+1,nc+1,7))
#ランダムなプロットに極大値（起点）を生成
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
#起点の値を周辺にぼかす
for (k in 1 :7){
for (l in 1 :b[k]){
for (i in 2 :nc){
for (j in 2 :nr){
	e.roundav<-(env[j-1,i-1,k]+env[j-1,i,k]+env[j-1,i+1,k]+env[j,i-1,k]+env[j,i,k]
				+env[j,i+1,k]+env[j+1,i-1,k]+env[j+1,i,k]+env[j+1,i+1,k])/8
	env[j,i,k]<-(env[j,i,k]+e.roundav)/2
	
}}}}
#1、101行目、1、101目を除いて100*100のプロットにする
env2<-array(dim=c(nr-1,nc-1,7))
for (i in 1 : 7){
	env2[,,i]<-env[2:101,2:101,i]
}
env.name<-c("Light","SOIL_MOIS","SOIL_DEP","SOIL_NIT","SOIT_CAR","AVE_MOIS","AVE_TWMP")


###真の存在範囲、観測データ作成
#調査サイト数
Nsite<-(nc-1)*(nr-1)
#各プロットの平均の個体数
int<-rep(1.5,(nc-1)*(nr-1))
##各調査個所の真の個体数
#オーダー調節
light<-as.vector(env2[,,1])/10
soilmois<-as.vector(env2[,,2])/2
soildep<-as.vector(env2[,,3])
soilnit<-as.vector( env2[,,4])
soilc<-as.vector(env2[,,5])
avemois<-as.vector(env2[,,6])/300
avetemp<-as.vector( env2[,,7])/300
#光、土壌水分、N量、C量、平均気温を線形結合、個体数を誤差分布ポアソン分布に従って生成
Nb<- rpois(Nsite,int- 2*light/max(light) +2*soilmois/max(soilmois) 
		-soilnit/max(soilnit)　+soilc/max(soilc) +avetemp/max(avetemp))

#座標付き真の個体数
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

#真の在不在データ
N.ex<-c(NULL)
for (i in 1:length(N[,1])){
	if (N[i,1]==0){
		N.ex[i]<-0
	}else{
		N.ex[i]<-1
}}

###観測値の作成
#観測値（不在には真の不在、偽の不在（未調査、発見できず）含む）
Nob<-array(dim=c(Nsite,3))
p<-c(NULL)
for (i in 1 : Nsite ){
#p:発見率。プロットの真の個体数と、プロットの調査が行われる確率に依存。
	p[i] <- (1+4/-(N[i,1]+4))**abs(1-rpois(n=1,lambda=2)/10)
	Nob[i,1]<-rbinom(1,1,p[i])
	Nob[i,2]<-N[i,2]
	Nob[i,3]<-N[i,3]
}

#観測値の在のみデータ作成
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

#データセットの図示
library(fields)
par(mfrow = c(3,3))
for (i in 1 : 7){
image.plot(env[,,i], col=terrain.colors(100),main=env.name[i])
}
image.plot(matrix(N[,1],ncol=100,nrow=100),col=terrain.colors(10),main="TRUE_NUNBER_OF_SPECIES")
plot(Nob.ex[,2:3],main="PLOT_OBSERVING_SPECIES")

#データ変換
oc<-Nob[,1]
pred<-data.frame(LIGHT=as.vector(env2[,,1]),SOIL_MOIS=as.vector(env2[,,2]),
	SOIL_DEP=as.vector(env2[,,3]),SOIL_NIT=as.vector(env2[,,4]),SOIL_C=as.vector(env2[,,5]),
	AVE_MOIS=as.vector(env2[,,6]),AVE_TEMP=as.vector(env2[,,7]))


###GLM
##在のみ観測データでGLM
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

##真の個体数データでGLM
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
#準備
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

##推定
# predict to entire dataset
	pred.result<- predict(me2,pred)
#推定結果の図示
ra<-array(pred.result,dim=c(100,100))
	image.plot(ra,col=terrain.colors(100))
	par(new=T) 
	plot(Nob.ex[,2:3],main="observe/predict")

##評価
#環境デ-タセットからback groundをランダムに抽出
pred.samp <- sample(nrow(pred),1000)
pred.bg <- pred[sort(pred.samp),]

#観測が確認されたプロットからテスト用に抽出
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

