x <- 0:5	 #グラフのx軸の範囲　
y <- c(447, 132, 42, 21, 3, 2)	 #死亡記事件数データ　
s <- sum(y)	 #データ総数　
m <- sum(x*y/s)	 #データ分布の平均　
v <- sum((x-m)^2*y/s)	 #データ分布の分散　
yp <- dpois(x, m)	 #平均 m のポアソン分布確率密度　
plot(x, y/s, type="h", ylab="確率")	 #データの棒グラフ表示　
points(x, yp, type="b", col="red")	 #ポアソン分布の重ねがき（赤）　
title(main="工事労働者の事故件数へのポアソン分布のあてはめ")
m
v
legend(3, 0.4, c("データ", "ポアソン分布"), lty=1, col=c("black","red"))



x <- 0:5	 #グラフのx軸の範囲　
y <- c(447, 132, 42, 21, 3, 2)	 #死亡記事件数データ　
s <- sum(y)	 #データ総数　
m <- sum(x*y/s)	 #データ分布の平均　
v <- sum((x-m)^2*y/s)	 #データ分布の分散　
np<- m/v
nn<- m*np/(1-np)
yp <- dnbinom(x, nn,np)	 #平均 m の負の二項分布確率密度　
plot(x, y/s, type="h", ylab="確率")	 #データの棒グラフ表示　
points(x, yp, type="b", col="red")	 #負の二項分布の重ねがき（赤）　
title(main="工事労働者の事故件数へのポアソン分布および負の二項分布のあてはめ")
ypb <- dpois(x, m)	 #平均 m のポアソン分布確率密度　
points(x, ypb, type="b", col="blue")	 #ポアソン分布の重ねがき（青）　
m
v
legend(3, 0.4, c("データ", "負の二項分布","ポアソン分布"), lty=1, col=c("black","red","blue"))








n <- 200	 # 個体数（予定）　
m <- 10	 # メッシュ（m2 個）　
p <- 4	 # 平均子ども数　
sig <- 0.05	 # 正規分布標準偏差　
xx <- NULL	 # xx（子ども座標）の定義　
xx0 <- NULL	 # xx0（親座標）の定義　
np <- round(n/p)	 # 親の数（round() は 5 捨 6 入）　
for(i in 1:np){	 # np 回の繰り返し　
x0 <- runif(2)	 # 親個体の座標を一様乱数で生成　
n0 <- rpois(1, p)	 # 子どもの数を平均 p のポアソン乱数で生成　
for(j in 1:n0){	 # n0 回の繰り返し　
xd <- rnorm(2, m=x0, sd=sig)	 # 子どもの座標を，正規乱数 N（x0, sig2）で生成
if(xd[1] > 1) xd[1] <- xd[1] - 1	 # x 座標が 1 を超えたとき区画の左端に
if(xd[1] < 0) xd[1] <- xd[1] + 1	 # x 座標が 0 未満のとき区画の右端に
if(xd[2] > 1) xd[2] <- xd[2] - 1	 # y 座標が 1 を超えたとき区画の下辺に
if(xd[2] < 0) xd[2] <- xd[2] + 1	 # y 座標が 0 未満のとき区画の上辺に
xx <- rbind(xx, xd)	 # 個体座標行列の行の追加　
xx0 <- rbind(xx0, x0)	 # 個体座標行列の行の追加　
}	 # 　
}	 # 　
# 区画内個体数	 # 　
x <- xx[,1]; y <- xx[,2]	 # x 座標ベクトル，y 座標ベクトル　
count <- NULL	 # count の定義　
for(i in 1:m){	 # m 回の繰り返し　
n1 <- (1:n)[x < (i-1)/m]	 # (i-1)/m 以下の乱数である番号　
n2 <- (1:n)[x < i/m]	 # i/m 以下の乱数である番号　
nin <- n2[!n2 %in% n1]	 # (i-1)/m から i/m の番号　
yy <- y[nin]	 # 上記 x 座標に対する y 座標
a <- hist(yy, breaks=0:m/m)	 # yy を 0 から 1 まで 1/m きざみで区切る　
count <- c(count, a$counts)	 # 区切った領域に入った個体の個数のベクトル
}	 # 　
mc <- max(count)	 # メッシュ内の個数の最大値　
xp <- 0:mc	 # 個数の定義域　
d <- factor(count, levels=xp)	 # 個数が 0 の階級も含める　
table(d)	 # メッシュ内個数の階級区分　
s <- sum(table(d))	 # 総個体数　
m1 <- sum(xp*table(d)/s)	 # カウント分布の平均　
v1 <- sum(table(d)/s*(xp-m1)^2)	 # カウント分布の分散　
#	 # 　
op <- par(mfrow = c(1, 2))	 # 横に２つのグラフを並べる　　
plot(x,y, col="red")	 # 個体分布の表示　
points(xx0, pch="+", col="green")	 # 個体分布の表示　
abline(h=0, v=0)	 # 外枠　
abline(h=1, v=1)	 # 外枠　
for(i in 1:m) abline(h=i/m, v=i/m, lty=3)	 # メッシュ区分線　
plot(xp, table(d)/s, type="h", ylim=c(0,0.35))	 # 区分された分布のグラフ　
yp <- dpois(xp, m1)	 # ポアソン分布確率　
points(xp, yp, type="b", col="red")	 # ポアソン分布表示　
nbp <- m1/v1	 # 　
nbn <- m1*nbp/(1-nbp)	 # 　
ynb <- dnbinom(xp, nbn, nbp)	 # 負の二項分布確率　
points(xp, ynb, type="b", col="green")	 # 負の二項分布表示　
legend(2.5, 0.33, c("data", "poisson", "negativebino"), lty=1, col=c("black","red","green"))
par(op)	 # 個体数　
title(main="区画内個数へのポアソン分布と負の二項分布のあてはめ")
m1	 # 平均　
v1	 # 分散　