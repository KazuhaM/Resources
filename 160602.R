vector1<-c(1:33)
vector1
vector2<-seq(99,0,by=-11)
vector2
vector3<-rep(0,100) 
vector3
x<-c()
for(i in 1:10){
	x<-c(x,rep(i,i))
}
x<-1:10
cor(x,vector2)
kisuu<-seq(1,100,by=2)
kisuu

#1～50の中の3の倍数
x<-c()
for(i in 1 : 50){
	if(i%%3==0){
		x<-c(x,i)	
	}
}
x

#1~100の1~100乗和
x<-1:100
sum(x^x)

y<-c(35.0, 38.0, 41.0, 15.0, 47.2, 36.5, 27.0, 
	38.0, 64.6, 34.0, 39.0, 41.0, 39.0, 37.3,
	51.5, 53.7, 54.0, 17.5, 32.5, 39.4, 35.0, 
	39.2, 42.0, 24.8, 27.1, 30.0, 25.3, 23.3,
	36.8, 38.7, 37.6, 38.6, 16.5, 34.5, 54.0,
	32.2, 39.2, 36.0, 34.9 )
summary(y)
hist(y)
var(y)
sd(y)


x<-"眠い！"
x


bt<-matrix(c(57,33,46,14,89,24,75,12),byrow=F,ncol=4)
bt
chisq.test(bt)$expected

# 回帰直線
#データの入力と図示
tannin <- c(0, 1, 2, 3, 4, 5, 6, 7, 8)
growth <- c(12, 10, 8, 11, 6, 7, 2, 3, 3)
plot(tannin, growth, xlab="タンニン濃度", ylab="成長速度")


# 回帰直線の決定
kaiki <- function(x, y, slope=0, show.res=FALSE) {
	plot(x, y)
	points(mean(x), mean(y), pch=4, col="red")
	intercept=mean(y)-slope*mean(x)
	abline(a=intercept, b=slope)
	y.predicted <- function(xx) intercept + slope*xx
	if(show.res){
		for(i in 1:length(x)){
			lines(c(x[i], x[i]), c(y[i], y.predicted(x[i])), lty=2)
		}
		return(sum((y.predicted(x)-y)^2))
	}
}
kaiki(tannin, growth, slope=0) 

kaiki(tannin, growth, slope=0, show.res=T)

kaiki(tannin, growth, slope=-0.5, show.res=T)

kaiki(tannin, growth, slope=-0.8, show.res=TRUE)

kaiki(tannin, growth, slope=-1.0, show.res=TRUE)

kaiki(tannin, growth, slope=-1.2, show.res=TRUE)

kaiki(tannin, growth, slope=-1.4, show.res=TRUE)

kaiki(tannin, growth, slope=-1.2166, show.res=TRUE)

#最小2乗法によるbとaの決定
tannin.me <- mean(tannin)
growth.me <- mean(growth)
b <- sum( (tannin-tannin.me)*(growth-growth.me) ) / sum( (tannin-tannin.me)^2 )
b
a <- growth.me - b*tannin.me
a

# lmを使う
model <- lm(growth ~ tannin)
model

# 傾きの検定
model <- lm(growth ~ tannin)
summary(model)
summary.aov(model)
p <- 1
0.8157 - (1-0.8157)*(p/(9 - p - 1))


# データの入力
weight <- c(60.8, 57.0, 65.0, 58.6, 61.7,
            68.7, 67.7, 74.0, 66.3, 69.8,
            102.6, 102.1, 100.2, 96.5,
            87.9, 84.2, 83.1, 85.7, 90.3)
feed <- as.factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4))
#　こちらの方がスマート↓
feed <- factor(c(rep(1, 5), rep(2, 5), rep(3, 4), rep(4, 5)))
feed
#feed<-c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4)


is.factor(feed)

is.factor(c(1, 2, 3, 4, 5))



#　feedをファクターに変換する
feed.fac <- as.factor(feed)
feed.fac
plot(feed.fac, weight, xlab="feed.fac", ylab="weight")



# 回帰分析での［全平方和］＝［回帰平方和］＋［誤差平方和］
# 分散分析の変動の分解
# 全平均のまわりでの体重の変動
plot(weight)
lines(c(1,19), c(mean(weight), mean(weight)))
for(i in 1:19)　lines(c(i,i), c(weight[i],mean(weight)),lty="dashed")

# 処理平均のまわりでの体重の変動
plot(weight)
lines(c(1,5),c(mean(weight[feed.fac=="1"]), mean(weight[feed.fac=="1"])))
lines(c(6,10),c(mean(weight[feed.fac=="2"]), mean(weight[feed.fac=="2"])))
lines(c(11,14),c(mean(weight[feed.fac=="3"]), mean(weight[feed.fac=="3"])))
lines(c(15,19),c(mean(weight[feed.fac=="4"]), mean(weight[feed.fac=="4"])))
for(i in 1:5) lines(c(i,i), c(weight[i],mean(weight[feed.fac=="1"])),lty="dashed")
for(i in 6:10) lines(c(i,i), c(weight[i],mean(weight[feed.fac=="2"])),lty="dashed")
for(i in 11:14) lines(c(i,i), c(weight[i],mean(weight[feed.fac=="3"])),lty="dashed")
for(i in 15:19) lines(c(i,i), c(weight[i],mean(weight[feed.fac=="4"])),lty="dashed")

# 全平均のまわりでの処理平均の変動
plot(weight, type="n")
lines(c(1,19), c(mean(weight), mean(weight))) # 全平均
# 処理平均
lines(c(1,5),c(mean(weight[feed.fac=="1"]), mean(weight[feed.fac=="1"])))
lines(c(6,10),c(mean(weight[feed.fac=="2"]), mean(weight[feed.fac=="2"])))
lines(c(11,14),c(mean(weight[feed.fac=="3"]), mean(weight[feed.fac=="3"])))
lines(c(15,19),c(mean(weight[feed.fac=="4"]), mean(weight[feed.fac=="4"])))
# 変動
lines(c(3,3), c(mean(weight),mean(weight[feed.fac=="1"])),lty="dashed")
lines(c(8,8), c(mean(weight),mean(weight[feed.fac=="2"])),lty="dashed")
lines(c(12.5,12.5), c(mean(weight),mean(weight[feed.fac=="3"])),lty="dashed")
lines(c(17,17), c(mean(weight),mean(weight[feed.fac=="4"])),lty="dashed")

#lm関数による分散分析
model <- lm(weight ~ feed)
model

#分散分析表の表示
summary.aov(model)

#係数の推定結果の表示
summary(model)


rats <- read.csv("rats.csv") 
rats
is.factor(rats$protein)
plot(data=rats,wgain ~ protein)

#　1元配置分散分析
model <- lm(wgain ~ protein, data=rats)
summary.aov(model)

attach(rats)

boxplot(rats$wgain ~ rats$protein)

boxplot(wgain ~ protein, data=rats)


# ボンフェローニの補正をした総当たりのｔ検定
pairwise.t.test(rats$wgain, rats$protein, p.adj="bonferroni")

#データ入力
weight <- c(30, 29, 31, 28, 38, 36, 23, 22, 26, 27, 36, 35)
feed <- as.factor(c(rep(rep(c("A", "B", "C"), times=c(2, 2, 2)), 2)))
sex <- as.factor(c(rep("male", 6), rep("female", 6)))
weight
feed
sex

#交互作用図
interaction.plot(feed, sex, weight)

#重回帰
fox <- read.csv("urban.foxes.csv")
fox
str(fox)

model00 <- lm(WEIGHT ~ AVFOOD + GSIZE + AREA, data=fox)
summary(model00)
model01 <- update(model00, .~. - AREA)
anova(model01, model00)
summary(model01)
model01

#共分散分析
#子羊の脂肪
#この例も交互作用が有意にならず、交互作用を除くとKindの効果が現れるので、モデル単純化の例題として良い。
goats <- read.csv("goats.csv")
str(goats)

#(2)
plot(Weightgain ~ Initial.wt, data=goats, type="n")
points(Weightgain[Treatment=="intensive"] ~ Initial.wt[Treatment=="intensive"], data=goats)
points(Weightgain[Treatment=="standard"] ~ Initial.wt[Treatment=="standard"], data=goats, pch=16)
abline(a=14.96661, b=-0.35137)
abline(a=14.96661-1.26486, b=-0.35137, lty=2)

#凡例
legend("topright", legend=c("Intensive", "Standard"), pch=c(1, 16), lty=c(1, 2))

#モデル単純化
model00 <- lm(Weightgain ~ Treatment * Initial.wt, data=goats)
summary(model00)
model01 <- update(model00, .~. - Treatment:Initial.wt)
anova(model01, model00)

summary(model01)
plot(goats$Weightgain ~ goats$Initial.wt, type="n")


whale<-read.csv("whale.watching.csv") 
str(whale)
whale$YEAR<-as.factor(whale$YEAR)
whale$MONTH<-as.factor(whale$MONTH)

names(whale)

model00<-lm(LRGWHAL ~  YEAR + MONTH + CLOUD8AM + RAIN8AM + VIS8AM ,data=whale)
summary(model00)
model01<-update(model00, .~. - RAIN8AM)
anova(model01,model00)
summary(model01)
model02<-update(model01, .~. - CLOUD8AM)
anova(model02,model01)
summary(model02)
















