#四分位数・箱ひげ図
futamon <- c(6.21, 6.24, 6.31, 6.26, 6.26, 6.35, 6.24, 6.25, 6.25, 
6.34, 6.17, 6.14, 6.14, 6.14, 6.23, 6.20, 6.22, 6.19, 6.18, 6.25)
futamon

#四分位数
quantile(futamon)
quantile(futamon, type=5)
quantile(futamon, type=1)
quantile(futamon, type=2)
quantile(futamon, type=3)
quantile(futamon, type=4)
quantile(futamon, type=5)
quantile(futamon, type=6)
quantile(futamon, type=7)
quantile(futamon, type=8)
quantile(futamon, type=9)

plot(futamon[order(futamon)])


#箱ひげ図
boxplot(futamon) #箱ひげ図を描く
boxplot(futamon, ylab="触角長")  #箱ひげ図のｙ軸にラベルをつける

#1標本のt検定（両側検定）
mean(futamon)

t.test(x = futamon, mu = 6.2)

xbar <- mean(futamon) #標本平均
s <- sd(futamon)　#標本標準偏差
n <- length(futamon)　#サンプルサイズ（length()関数はベクトルの要素を数える関数）

xbar
s
n

t <- (xbar - 6.2)/(s/sqrt(n))
t

#有意水準と棄却域の図（両側検定）
xx <- seq(-6, 6, 0.01)
plot(xx, dt(x=xx, df=19), type="l", axes=F, xlab="", ylab="")
abline(h=0)
abline(v=qt(0.025, df=19), lty="dashed")
abline(v=qt(0.975, df=19), lty="dashed")
xxx1 <- seq(-6, qt(0.025, df=19), 0.01)
polygon(c(xxx1, rev(xxx1)), c(rep(0, length(xxx1)), dt(rev(xxx1), df=19)), col="red")
xxx2 <- seq(qt(0.975, df=19), 6, 0.01)
polygon(c(xxx2, rev(xxx2)), c(rep(0, length(xxx2)), dt(rev(xxx2), df=19)), col="red")
#有意確率の領域
xxx3 <- seq(2.5, 6, 0.01)
polygon(c(xxx3, rev(xxx3)), c(rep(0, length(xxx3)), dt(rev(xxx3), df=19)), col="yellow")
xxx4 <- seq(-6, -2.5, 0.01)
polygon(c(xxx4, rev(xxx4)), c(rep(0, length(xxx4)), dt(rev(xxx4), df=19)), col="yellow")

#有意確率の計算
2*(1 - pt(t, df=19))
#ptはt分布の累積分布関数。pt(t, df=19)とすると、自由度19のt分布の-∞からtまでの範囲の確率を計算できる。

xx <- seq(-6, 6, 0.01)
plot(xx, dt(x=xx, df=19), type="l", axes=F, xlab="", ylab="")
abline(h=0)
xxx5 <- seq(-6, 2, 0.01)
polygon(c(xxx5, rev(xxx5)), c(rep(0, length(xxx5)), dt(rev(xxx5), df=19)), col="lightblue")

#1標本のt検定（片側検定）
hamadango <- c(35.0, 38.0, 41.0, 15.0, 47.2, 36.5, 27.0, 38.0, 64.6, 34.0, 39.0, 41.0, 
39.0, 37.3, 51.5, 53.7, 54.0, 17.5, 32.5, 39.4, 35.0, 39.2, 42.0, 24.8, 
27.1, 30.0, 25.3, 23.3, 36.8, 38.7, 37.6, 38.6, 16.5, 34.5, 54.0, 32.2, 
39.2, 36.0, 34.9)

hist(hamadango)
boxplot(hamadango)

mean(hamadango)

t.test(x = hamadango, mu = 30, alternative = "greater")

#片側検定の棄却域
xx <- seq(-6, 6, 0.01)
plot(xx, dt(x=xx, df=19), type="l", axes=F, xlab="", ylab="")
abline(h=0)
abline(v=qt(0.95, df=19), lty="dashed")
xxx1 <- seq(qt(0.95, df=19), 6, 0.01)
polygon(c(xxx1, rev(xxx1)), c(rep(0, length(xxx1)), dt(rev(xxx1), df=19)), col="red")

xxx2 <- seq(2, 6, 0.01)
polygon(c(xxx2, rev(xxx2)), c(rep(0, length(xxx2)), dt(rev(xxx2), df=19)), col="yellow")

#独立2標本のｔ検定
#データの視覚化
tone <- c(165, 130, 182, 178, 194, 206, 160, 122, 212, 165, 247, 195)
shina <- c(180, 180, 235, 270, 240, 295, 164, 152)

#複数グループのデータを比較するには、箱ひげ図が便利
boxplot(tone, shina)
#x軸目盛をグループ名にして、y軸ラベルをつけるには
boxplot(tone, shina, names=c("利根川", "信濃川"), ylab="体長")
#箱ひげの色を変えたかったら
boxplot(tone, shina, names=c("利根川", "信濃川"), ylab="体長", 
col=c("lightblue", "pink"))

#独立2標本のｔ検定：t.test
t.test(x = tone, y = shina)

#対応のあるｔ検定
denshi <- c(37.1, 36.2, 36.6, 37.4, 36.8, 36.7, 36.9, 37.4, 36.6, 36.7)
suigin <- c(36.8, 36.6, 36.5, 37.0, 36.0, 36.5, 36.6, 37.1, 36.4, 36.7)
boxplot(denshi, suigin, names=c("電子体温計", "水銀体温計"), ylab="測定体温")

#対応のあるデータであることを考えると
boxplot(denshi - suigin, ylab="電子体温計と水銀体温計の測定値の差")
abline(h=0, lty=2)

#対応のあるt検定でのt.testの使い方
t.test(x = denshi, y = suigin, paired = TRUE, alternative = "greater")


