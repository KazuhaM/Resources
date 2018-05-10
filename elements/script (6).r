###環境データセット作成
##環境データ７つ（光、土壌水分、土壌厚、N量、C量、平均湿度、平均気温）

#プロット数
nc<-101
nr<-101
#環境データセット作成、各条件について正規分布に従う乱数で値を生成
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
#各プロットにの値を、上下左右のデータをもとに計算
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

#一行目、一列目を除いて100*100のプロットにする
env2<-array(dim=c(nr-1,nc-1,7))
for (i in 1 : 7){
	env2[,,i]<-env[1:100,1:100,i]
}
#別手法
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
#一,百一行目、一,百一列目を除いて100*100のプロットにする
env2<-array(dim=c(nr-1,nc-1,7))
for (i in 1 : 7){
	env2[,,i]<-env[2:101,2:101,i]
}

###真の存在範囲、観測データ作成
#調査サイト数
Nsite<-(nc-1)*(nr-1)
#各プロットの平均の個体数
int<-rep(1,10000)
##各調査個所の真の個体数
#光、土壌水分、N量、平均気温を用いて線形、個体数をポアソン分布に従って生成
light<-as.vector(env2[,,1])
soilwet<-as.vector(env2[,,2])
nit<-as.vector( env2[,,4])
avetemp<-as.vector( env2[,,7])
Nb<- rpois(Nsite,exp(1/(1+abs(int- 2*light +2*soilwet -nit +avetemp)/100)))
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

###観測値の作成
#観測値（不在には真の不在、偽の不在（未調査、発見できず）含む）
Nob<-array(dim=c(Nsite,3))
for (i in 1 : Nsite ){
#p:発見率。プロットの真の個体数と、プロットの調査が行われる確率に依存。
	p[i] <- atan(((N[i,1]*runif(n=1,0,1))/(max(N[,1])+0.1))*pi/2)  
	Nob[i,1]<-rbinom(1,1,p[i])
	Nob[i,2]<-N[i,2]
	Nob[i,3]<-N[i,3]
}

#在のみデータ作成
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

#データセットの図示
par(mfrow = c(3,3))
for (i in 1 : 7){
image(env[,,i], col=terrain.colors(100))
}
image(matrix(N[,1],ncol=100,nrow=100),col=terrain.colors(5))
plot(N2[,2:3])


#データ変換
oc<-Nob[,1]
pred<-data.frame(LIGHT=as.vector(env2[,,1]),SOIL_MOIS=as.vector(env2[,,2]),
	SOIL_DEP=as.vector(env2[,,3]),SOIL_NIT=as.vector(env2[,,4]),SOIL_C=as.vector(env2[,,5]),
	AVE_MOIS=as.vector(env2[,,6]),AVE_TEMP=as.vector(env2[,,7]))

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

##評価
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


#テスト用
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










