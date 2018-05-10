summary(d)
sd(d)

### 分割数設定　任意の数に変更してください　###
gnum_st<-5 ##地点
gnum_sp<-5　##種


### データ読み込み　###
library(vegan) 
library(labdsv) #indval用
d<-read.csv("WE_Veg_over3times.csv",row.names=1)
#d[is.na(d)]<-0
d<-t(d)
#d<-t(d)
col<-ncol(d)
row<-nrow(d)
is.numeric(d)
d

# DCAの場合
d.mds<-decorana(d)
#MDSの場合
d.mds<-metaMDS(d,zerodist="add") ##### zerodist="add" を追加
d.mds
summary(d.mds)
stressplot(d.mds)
#PCAの場合
d.mds<-prcomp(d)
d.mds
plot(d.mds)
summary(d.mds)
biplot(d.mds,choices=1:2)
biplot(d.mds)

##### クラスター分析 ####
sco<-d.mds$points　#序列化された座標値
km <- cascadeKM(sco, 2, 6) #kmeans法による非階層クラスタ分析 2～6分割を試す
plot(km) #分割の結果を確認　Calinski-Harabasz の基準
#↑の結果で最適な分割数を決定↓に反映
dnum<-4 #変数に分割数を代入
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
ordipointlabel(d.mds,display="sites",col = (kms+1)[memb],pch = kms[memb],cex=1.3)
ordiellipse(d.mds,memb, display="sites",kind="sd", conf=0.9,lwd=1,lty=2, col="black")
x<-tapply(sco[,1],memb,mean) #グループの重心
y<-tapply(sco[,2],memb,mean) 
par(new=T)
text(x,y,n)　#グループごとに指標種を表示
dul #指標種の一覧を確認

mrpp(d,memb)

### クラスタリング ###
sco<-scores(d.mds,display="sites") #地点の序列化スコア
clus<-hclust(dist(sco),"ward.D2") #ウォード法で
#clus<-hclust(dist(sco),"average") #群平均法で
par(ps=5.5)
plot(clus,cex=1.3) #グラフ表示
rect.hclust(clus,gnum_st,)
clus
summary(clus)

### グループ分けした序列を　グラフ表示 ###
windows()
k<-cutree(clus,gnum_st)
          #####　以下の1行プログラムミスがあったので修正 #####
ordipointlabel(d.mds,display="sites",col = c(2:(gnum_st+2))[k],pch = c(1:gnum_st+1)[k])
ordiellipse(d.mds,k, display="sites",kind="sd", conf=0.9,lwd=1,lty=2, col="black")

##### 指標種分析 #####
dul<-indval(d,memb) #labdsvを利用
duli<-dul$indval　#indvalの値
kms<-c(1:dnum)
for( i in n<-seq(length(duli))) n[i]<-rownames(duli)[duli[,i]==max(duli[,i])] #地点の最高指標種
#指標種結果の整形　参考　http://kobe.cool.ne.jp/matsut/r38.txt
dul<-data.frame(numeric.sp=names(dul$maxcls), community=dul$maxcls, indval=round(dul$indcls,3), pvalue=round(dul$pval,4))
dul<- dul[order(dul$community, dul$indval, dul$pvalue, decreasing=T),][,-1]

### 地点の分割数ごとにリストの並び替えと指標種を計算 ###
#変数初期化
li<-list(NULL)
me<-list(NULL)
ordtxt<-NULL
d2<-d #出力のためのデータ

#最大数に分割したときの順番を取得
tmp<-cbind(clus$order,c(1:row))
tmp<-tmp[order(tmp[,1]),]
tmp<-tmp[,2]

#分割数分だけループ
for( i in 1:gnum_st) {
 ifelse(i==gnum_st,me[[i]]<-tmp,me[[i]]<-cutree(clus,i+1))
 ordtxt[i]<-paste("me[[",i,"]]")#orderのための文字列
　d2<-cbind(d2,me[[i]])
 colnames(d2)[col+i]<-paste("group",i+1,sep="")

 dul<-indval(d,me[[i]]) #指標種分析 
 dul<-data.frame(numeric.sp=names(dul$maxcls), community=dul$maxcls, indval=round(dul$indcls,3), pvalue=round(dul$pval,4))
 dul<- dul[order(dul$community, dul$indval, dul$pvalue, decreasing=T),][,-1]
 li[[i]]<-dul #指標種分析結果
}
ord<-eval(parse(text=paste("order(",paste(ordtxt,collapse=","),")")))
d2<-d2[ord,]#並び替え

d2<-t(d2) ###地点、種入れ替え

### 種も同様に並び替え　###
sco<-scores(d.mds,display="species")
clus<-hclust(dist(sco),"average")
me<-list(NULL)
ordtxt<-NULL
tmp<-cbind(clus$order,c(1:col)) #クラスターの順番を表の並び替えに利用するため
tmp<-tmp[order(tmp[,1]),]
tmp<-tmp[,2]

for( i in 1:gnum_sp) {
 ifelse(i==gnum_sp,me[[i]]<-tmp,me[[i]]<-cutree(clus,i+1))
 ordtxt[i]<-paste("me[[",i,"]]")#orderのための文字列
　d2<-cbind(d2,me[[i]])
 colnames(d2)[row+i]<-paste("group",i+1,sep="")
}

# 並び替えの前に　必要ない行と列にNA代入
for(i in 1:gnum_st){
 for(j in 1:gnum_sp){
  d2[col+i,row+j]<-NA
 }
}
ord<-eval(parse(text=paste("order(",paste(ordtxt,collapse=","),")")))
d2<-rbind(d2[ord,],d2[(col+1):(col+gnum_st),])#並び替え

d2<-d2[,-(row+gnum_sp)]　#並び替えのための最終列を削除
d2<-d2[-(col+gnum_st),] #並び替えのための最終行を削除

### 並び替えた結果と指標種を書き出し
write.table(d2,"結果.csv",sep=",",row.names=T,col.names=T)
for(i in 1:gnum_st) write.table(li[[i]],paste("indval",i,".csv",sep=""),sep=",",row.names=T,col.names=T)