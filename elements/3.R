品種収量の分散分析
n <- nrow(hinsyu)
n
m <- ncol(hinsyu)
m
boxplot(hinsyu)
idname <- rep(names(hinsyu), each=n)
idname
hinsyu.vec <- as.vector(as.matrix(hinsyu))
hinsyu.fr <- data.frame(data = hinsyu.vec, id = idname)
table(hinshinsyu <- read.csv("hinsyu.csv"); hinsyu	# csv データ読み込み
yu.fr$id)
av <- aov(hinsyu.fr$data ~ hinsyu.fr$id)		# 分散分析
summary(av)
hinsyu.fr$data
hinsyu.fr$id

pairwise.t.test(hinsyu.fr$data, hinsyu.fr$id)  	# 対比較ホルム補正 
pairwise.t.test(hinsyu.fr$data, hinsyu.fr$id, p.adj = "bonf")  # 対比較ボンフェローニ補正
pairwise.t.test(hinsyu.fr$data, hinsyu.fr$id, p.adj = "none")  # 対比較補正なし
TukeyHSD(av)  	# チューキー HSD 　



a <- 5; n <- 10  # 処理水準数 a，処理内標本数 n 　 
hinsyu.fr$id <- NULL  # 　 
for(i in 1:a) x <- c(x, rep(i, n))  # グループラベル 　
x
x <- factor(x)		# ラベル化
x
y <- rnorm(n*a) 
y
cbind(y, x)

av <- aov(y ~ x)		# 分散分析
summary(av)

pairwise.t.test(y, x)  	# 対比較ホルム補正 
pairwise.t.test(y, x, p.adj = "bonf")  # 対比較ボンフェローニ補正
pairwise.t.test(y, x, p.adj = "none")  # 対比較補正なし
TukeyHSD(av)  	# チューキー HSD 　



pis <- read.csv("barcode.csv")
pis
boxplot(pis)
dy1<-pis[,1]
dy2<-pis[,8]
dy<-c(dy1,dy2)
sun<-c(pis[,2],pis[,7])
ms<-pis[,3]
fm<-pis[,4]
law<-c(pis[,5],pis[,10])
se<-c(pis[,6],pis[,9])
se
id<-NULL
for(i in 1:6){if(i==3 || 4)
	for(j in 1:83){
		id<-c(id,j)}
	else for(k in 1:166){
		id<-c(id,k)}
}
id<-NULL
for(k in 1:166){
		id<-c(id,"se")}
id
for(j in 1:83){
		id<-c(id,"1")}
id
data1<-cbind(dy,sun,ms,fm,law,se)
data<-c(dy,sun,ms,fm,law,se)
pis.fr <- data.frame(data = data1, id = idname)

av <- aov(data ~ id)
av
pairwise.t.test(data,id) 

iddy<-NULL
for(j in 1:83){
		iddy<-c(iddy,"1")}
for(j in 1:83){
		iddy<-c(iddy,"2")}
iddy
avd <- aov(dy ~ iddy)
avs <- aov(sun ~ iddy)
avl <- aov(law ~ iddy)
avse <- aov(se ~ iddy)
avd
pairwise.t.test(dy,iddy)
pairwise.t.test(sun,iddy)
pairwise.t.test(law,iddy)
pairwise.t.test(se,iddy)
dy
law
iddy 
pis




品種収量の分散分析
pis <- read.csv("pis.csv"); pis	# csv データ読み込み
n <- nrow(pis)
m <- ncol(pis)
boxplot(pis)
idname <- rep(names(pis), each=n)
pis.vec <- as.vector(as.matrix(pis))
pis.fr <- data.frame(data = pis.vec, id = idname)
table(pis.fr$id)
av <- aov(pis.fr$data ~ pis.fr$id)		# 分散分析
summary(av)
pairwise.t.test(pis.fr$data, pis.fr$id)[




rice <- read.csv("ricecul.csv") 	# データ読み込み
rice
yield <- rice$gy 
years <- factor(rice$year)
dense <- factor(rice$density)		# 水準のラベル化
fert <- factor(rice$fert)
blk <- factor(rice$rep)  
tapply(yield[1:12], dense[1:12], mean)		# 密度水準ごとの平均
tapply(yield[1:12], fert[1:12], mean) 
tapply(yield[1:12], blk[1:12], mean) 
cm <- tapply(yield[1:12], dense[1:12]:fert[1:12], mean); cm	# 処理組み合わせごとの平均
# 施肥量水準に対する収量のグラフ
plot(1:3, cm[1:3], type="b", lwd=2, cex=1.5, xaxt="n", xlab="fertility", ylab="yield",  
ylim=c(300, 600), pch=0, cex.lab=1.0, cex.axis=1.0, col="blue") 
axis(1, 1:3, labels=c("fert1","fert2","fert3"), cex.axis=0.8) 
points(1:3, cm[4:6], type="b", lwd=2, cex=1.5, pch=2, col="red")
legend(1.2, 580, legend=c("density1","density2"), pch=c(0,2), 
col=c("blue","red"), cex=0.8)  
title(main="施肥量水準に対するコメ収量（2000年）")  
# 分散分析
ry.aov <- aov(yield[1:12] ~ dense[1:12])
summary(ryaov)                 #分散分析表の表示

rice$year <- factor(rice$year)
rice$year
years
rice$fert <- factor(rice$fert)
rice$density <- factor(rice$density)
rice$rep <- factor(rice$rep)  

aov1 <- aov(gy~(year+density+fert)^2,data=rice)
summary(aov1)




rice <- read.csv("ricecul.csv") 	# データ読み込み
rice
rice$sn <- rice$sn
sn<-rice$sn
rice$year <- factor(rice$year)
rice$fert <- factor(rice$fert)
rice$density <- factor(rice$density)  
tapply(yield[1:12], dense[1:12], mean)		# 密度水準ごとの平均
tapply(yield[1:12], fert[1:12], mean) 
tapply(yield[1:12], blk[1:12], mean) 
cm <- tapply(yield[1:12], dense[1:12]:fert[1:12], mean); cm	# 処理組み合わせごとの平均
# 施肥量水準に対する収量のグラフ

ry.aov <- aov(sn[1:12] ~(dense[1:12] + fert[1:12])^2)
summary(ry.aov)                 #分散分析表の表示