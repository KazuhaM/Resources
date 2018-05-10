library(vegan)
library(labdsv)

##### データの準備 #####
d<-read.table("twinspan4.txt",header=T)#データ読み込み
d<-data.matrix(d)#行列に変換
d[which(d==1,arr.ind=TRUE)]<-0 #出現→1、非出現→0に
d[which(d==2,arr.ind=TRUE)]<-1　
spc<-specnumber(d)　#確認地点数を確認
spc<-names(spc[spc!=0]) #1回でも出た種
d<-d[match(spc,rownames(d)),] #1回も出ていない種を削除
d<-t(d) #行列入れ替え
specnumber(d) #種数を確認
#d<-subset(d,rownames(d)!="st1") #はずれ値の地点を除外。今回は無し

d<-
##### 序列化 非計量多次元尺度構成法 ####
d.mds<-metaMDS(d,dist="bray") #在・不在データなのでjaccard数値ならbray　
stressplot(d.mds) #MDSの当てはまりの良さ確認

##### クラスター分析 ####
sco<-d.mds$points　#序列化された座標値
km <- cascadeKM(sco, 2, 6) #kmeans法による非階層クラスタ分析 2～6分割を試す
plot(km) #分割の結果を確認　Calinski-Harabasz の基準
#↑の結果で最適な分割数を決定↓に反映
dnum<-5 #変数に分割数を代入
memb<-km$partition[,dnum-1] #グループがどのように分割されたかを取得 

##### 指標種分析 #####
dul<-indval(d,memb) #labdsvを利用
duli<-dul$indval　#indvalの値
kms<-c(1:dnum)
for( i in n<-seq(length(duli))) n[i]<-rownames(duli)[duli[,i]==max(duli[,i])] #地点の最高指標種
#指標種結果の整形　参考　http://kobe.cool.ne.jp/matsut/r38.txt
dul<-data.frame(numeric.sp=names(dul$maxcls), community=dul$maxcls, indval=round(dul$indcls,3), pvalue=round(dul$pval,4))
dul<- dul[order(dul$community, dul$indval, dul$pvalue, decreasing=T),][,-1]

##### 作図 #####
ordipointlabel(d.mds,display="sites",col = (kms+1)[memb],pch = kms[memb])
ordiellipse(d.mds,memb, display="sites",kind="sd", conf=0.9,lwd=1,lty=2, col="black")
x<-tapply(sco[,1],memb,mean) #グループの重心
y<-tapply(sco[,2],memb,mean) 
par(new=T)
text(y~x,n)　#グループごとに指標種を表示
dul #指標種の一覧を確認