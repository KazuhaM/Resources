### データ読み込み　###
library(vegan) 
library(labdsv) #indval用
d<-read.csv("pspe.csv",row.names=1)
d[is.na(d)]<-0
d<-t(d)
d<-t(d)
col<-ncol(d)
row<-nrow(d)
is.numeric(d)
d

### 分割数設定　任意の数に変更してください　###
gnum_st<-5 ##地点
gnum_sp<-6　##種

# DCAの場合
d.mds<-decorana(d)
summary(d.mds)

#MDSの場合
d.mds<-metaMDS(d,zerodist="add") ##### zerodist="add" を追加
stressplot(d.mds) # Shepard diagram を描く。
d.mds
plot(d.mds,type="t")
summary(d.mds)

#CCAの場合
#e<-read.csv("envirnment.csv",row.names=1)
#e[is.na(e)]<-0
#e<-t(e)
#is.numeric(e)
#d.cca<-cca(d,e)
#d.cca
#plot(d.cca)

#PCAの場合
#d.mds<-prcomp(d)
#d.mds
#plot(d.mds)
#summary(d.mds)
#biplot(d.mds,choices=1:2)


### クラスタリング ###
sco<-scores(d.mds,display="sites") #地点の序列化スコア
clus<-hclust(dist(sco),"ward") #ウォード法で
#clus<-hclust(dist(sco),"average") #群平均法で
par(ps=5.5)
plot(clus,cex=2) #グラフ表示
rect.hclust(clus,gnum_st,)
clus
summary(clus)

#二元配置分散分析
e<-read.csv("alobj.csv",row.names=1)
	e.r<-e[,1]
	e.sad<-e[,2]
	e.sae<-e[,3]
f<-read.csv("alexp.csv",row.names=1)
	f.f<-f[,1]
	f.c<-f[,2]
	f.t<-f[,4]
interaction.plot(f.t , f.f, e.r )
	title("all| Richness ~ e/ey/en f/m")
e.var<-lm(e.r ~ f.t + f.f + f.t:f.f)
summary.aov(e.var)

#一元配置分散分析
e<-read.csv("alobj.csv",row.names=1)
	e.r<-e[,1]
	e.sad<-e[,2]
	e.sae<-e[,3]
f<-read.csv("alexp.csv",row.names=1)
	f.f<-f[,1]
	f.c<-f[,2]
	f.t<-f[,4]
plot(e.r ~f.t)
	title("all| Richness ~ e/ey/en")
e.var<-lm(e.r ~ f.t)
summary.aov(e.var)


plot(f.f, e.sad, xlab="平地/山", ylab="Shannon Div")



#希薄化
# 読み込み
ra <- read.csv("rare.csv", header=T) 
paddy <- rep(ra$species,ra$paddy) 
ye <- rep(ra$species,ra$e)  

# ヒストクラム
ye.sum<-sum(ra$e)
resamp <- sapply(1:1000, function(x) length( levels( factor( sample(paddy,ye.sum ) ) ) ))
yepar <- par(no.readonly = TRUE)
par(mar=c(5,5,0,0))
hist(resamp, main="", xlab="種数", ylab="シミュレーション数", col="gray")
par(yepar) 

# 希薄化(Rarefaction)
num.resamp <- 1000 # 繰り返し数
## Rarefaction for 田
out <- quantile(1, c(0.025, 0.5, 0.975)) 
for(i in 1:length(paddy)){ 
	resamp <- sapply(1:num.resamp, function(x) length( levels( factor( sample(paddy, i) ) ) )) 
	out <- rbind(out, quantile(resamp, c(0.025, 0.5, 0.975))) 
} 
## Rarefaction for 江
out2 <- quantile(1, c(0.025, 0.5, 0.975)) 
for(i in 1:length(ye)){ 
	resamp <- sapply(1:num.resamp, function(x) length( levels( factor( sample(ye, i) ) ) )) 
	out2 <- rbind(out2, quantile(resamp, c(0.025, 0.5, 0.975))) 
} 

## 希薄化曲線の Plot
yepar <- par(no.readonly = TRUE)
par(mar=c(5,5,1,1))
plot(out[,2], type="l", xlab="個体数", ylab="種数", xlim=c(0, 1300),ylim=c(0,30))
points(out[,1], type="l", lty="dashed")
points(out[,3], type="l", lty="dashed")
points(out2[,2], type="l", col="red")
points(out2[,1], type="l", lty="dashed", col="red")
points(out2[,3], type="l", lty="dashed", col="red")
text(c(700, 800), c(25, 17), labels=c("江", "田"), col=c("red", "black") ) 
par(yepar) 




#山・平、江有り無しの計四種比較
pob<-read.csv("pobj.csv",row.names=1)
#Tukey-Kramer test
score<-pob[,4]
group<-factor(pob[,3])
score
group
bartlett.test(score ~ group)
TukeyHSD(aov(score ~ group))

#田江、江有り無し３種比較
pob<-read.csv("alobj.csv",row.names=1)
pobg<-read.csv("alexp.csv",row.names=1)
#Tukey-Kramer test
score<-pob[,1]
group<-factor(pobg[,4])

bartlett.test(score ~ group)
TukeyHSD(aov(score ~ group))


boxplot(score ~ group)
groupf<-factor(pob[,1])

boxplot(score ~ groupf)
	title("paddy f/m")

groupe<-factor(pob[,2])
boxplot(score ~ groupe)
	title("paddy y/n")

pob2<-read.csv("pob2.csv")
t.test(pob2[,1],pob2[,2])
t.test(pob2[,4],pob2[,5])

kairo<-read.csv("kairo.csv",header=T)
t.test(kairo[,47],kairo[,48],var.equal=F)
var.test(kairo[,47],kairo[,48])
boxplot(kairo[,47],kairo[,48])

eexp<-read.csv("eexp.csv",header=T)
boxplot(eexp[,11],eexp[,2])
ee1<-c(eexp[1,11],eexp[2,11])
ee2<-c(eexp[3:8,11])
t.test(ee1,ee2,var.equal=F)