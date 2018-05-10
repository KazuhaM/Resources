# F分布
xx <- seq(0, 6, 0.01)
plot(xx, df(xx, df1=2, df2=10), type="l")
points(xx, df(xx, df1=3, df2=15), type="l", col="blue")
points(xx, df(xx, df1=5, df2=30), type="l", col="red")


# データの入力
weight <- c(60.8, 57.0, 65.0, 58.6, 61.7,
            68.7, 67.7, 74.0, 66.3, 69.8,
            102.6, 102.1, 100.2, 96.5,
            87.9, 84.2, 83.1, 85.7, 90.3)
feed <- as.factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4))
#　こちらの方がスマート↓
feed <- factor(c(rep(1, 5), rep(2, 5), rep(3, 4), rep(4, 5)))
feed

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


# 分散分析表
anova(lm(weight ~ feed.fac))

# Ｆ分布の棄却域
qf(0.95, df1=3, df2=15)

# 処理平均の間に有意な違いがない場合
saplings <- read.table("Saplings.txt", header=T)
str(saplings)
attach(saplings)
water <- as.factor(WATER)
plot(FINALHT ~ water)
summary(water)

#全平方和
plot(FINALHT)
lines(c(1,40), c(mean(FINALHT), mean(FINALHT)))
for(i in 1:40)　lines(c(i,i), c(FINALHT[i],mean(FINALHT)),lty="dashed")

# 処理平均のばらつき
plot(FINALHT, type="n")
lines(c(1, 40), c(mean(FINALHT), mean(FINALHT)))
lines(c(1, 10), c(mean(FINALHT[water=="1"]), mean(FINALHT[water=="1"])))
lines(c(11, 20), c(mean(FINALHT[water=="2"]), mean(FINALHT[water=="2"])))
lines(c(21, 30), c(mean(FINALHT[water=="3"]), mean(FINALHT[water=="3"])))
lines(c(31, 40), c(mean(FINALHT[water=="4"]), mean(FINALHT[water=="4"])))
lines(c(5.5, 5.5), c(mean(FINALHT), mean(FINALHT[water=="1"])), lty=2)
lines(c(15.5, 15.5), c(mean(FINALHT), mean(FINALHT[water=="2"])), lty=2)
lines(c(25.5, 25.5), c(mean(FINALHT), mean(FINALHT[water=="3"])), lty=2)
lines(c(35.5, 35.5), c(mean(FINALHT), mean(FINALHT[water=="4"])), lty=2)

# 処理平均の周りの誤差
plot(FINALHT)
lines(c(1, 10), c(mean(FINALHT[water=="1"]), mean(FINALHT[water=="1"])))
lines(c(11, 20), c(mean(FINALHT[water=="2"]), mean(FINALHT[water=="2"])))
lines(c(21, 30), c(mean(FINALHT[water=="3"]), mean(FINALHT[water=="3"])))
lines(c(31, 40), c(mean(FINALHT[water=="4"]), mean(FINALHT[water=="4"])))
for(i in 1:10) lines(c(i, i), c(mean(FINALHT[water=="1"]), FINALHT[i]), lty=2)
for(i in 11:20) lines(c(i, i), c(mean(FINALHT[water=="2"]), FINALHT[i]), lty=2)
for(i in 21:30) lines(c(i, i), c(mean(FINALHT[water=="3"]), FINALHT[i]), lty=2)
for(i in 31:40) lines(c(i, i), c(mean(FINALHT[water=="4"]), FINALHT[i]), lty=2)


## 1. 作業ディレクトリの変更
## 1.1 [R Console]ウィンドウを選択した状態で、［ファイル］→［ディレクトリの変更］
## 1.2 ［フォルダ：］の右の空欄に自分の作業ディレクトリを表示させる→［OK］

## 2. 新しいスクリプトの作成
## 2.1 [R Console]ウィンドウを選択した状態で、［ファイル］→［新しいスクリプト］
## 2.2 新しいスクリプトウィンドウが開いたら、適当な名前（xxx.R）をつけて保存（「.R」という拡張子をつけること）

## 作業ディレクトリの中にデータファイル（rats.txt）をダウンロードしておく

################
### 多重比較　###
################

# データを読み込んで表示・図示
# rats.txtの中身をデータフレームとして読み込み、ratsという名前をつける
rats <- read.table("rats.txt", header=T) 
# ratsの内容を表示
rats
# ratsの各列の名前を表示
names(rats) 

is.factor(protein) #　データフレーム内の列の要素が文字列で与えられている場合、その列はファクターと見なされる
plot(wgain ~ protein)

#　1元配置分散分析
model <- lm(wgain ~ protein, data=rats)
summary.aov(model)

attach(rats)

boxplot(rats$wgain ~ rats$protein)

boxplot(wgain ~ protein, data=rats)



### ｔ分布の棄却域　###
x <- seq(-5,5,0.01)
plot(x, dt(x, df=10), type="l")
lowerx <- seq(-5, qt(0.025, df=10), 0.01)
polygon(c(lowerx,rev(lowerx)), c(dt(lowerx,df=10),rep(0, length(lowerx))), col="red")
upperx <- seq(qt(0.975, df=10), 5, 0.01)
polygon(c(upperx,rev(upperx)), c(dt(upperx, df=10), rep(0, length(upperx))), col="red")

# ボンフェローニの補正をしない総当たりのｔ検定
pairwise.t.test(rats$wgain, rats$protein, p.adj="none")

# ボンフェローニの補正をした総当たりのｔ検定
pairwise.t.test(rats$wgain, rats$protein, p.adj="bonferroni")


#係数推定
summary(model)

levels(rats$protein) <- c("beef.high", "cereal.high", "beef.low", "cereal.low")



