m <- 50	# 標識をつけた数　
n <- 450	# 標識をつけられていない数　
k <- 50	# 再捕獲数　
estimate <- NULL	# 個体数推定値の定義　
for (i in 1:10000) {	# 10000回のシミュレーション　
x <- rhyper(1, m, n, k)	# 超幾何分布に従う乱数１つ抽出　
estimate <- c(estimate, k * m / x)	# 個体数推定値列ベクトル　
}	#　
hist(estimate, breaks=seq(0, 2600, by=100), main="")	#推定値のヒストグラム　
title(main="Distribution of habitat estimates ")	 #タイトル　
table(estimate)	 #推定値の階級分け　
quantile(estimate, c(0.05, 0.1, 0.5, 0.9, 0.95))	 # 分位点（パーセンタイル）　

o <- NULL
for(m in 1:99){　
n <- 500-m	# 標識をつけられていない数　
k <- 100-m	# 再捕獲数　
estimate <- NULL	# 個体数推定値の定義　
for (i in 1:10000) {	# 10000回のシミュレーション　
x <- rhyper(1, m, n, k)	# 超幾何分布に従う乱数１つ抽出　
if(x !=0){
estimate <- c(estimate, k * m / x)
}
} # 個体数推定値列ベクトル
me <- median(estimate)　
o <- c(o, me)
}     #
o
plot(o)



o <- NULL
for(m in 1:49){　
n <- 500-m	# 標識をつけられていない数　
k <- 50-m	# 再捕獲数　
estimate <- NULL	# 個体数推定値の定義　
for (i in 1:10000) {	# 10000回のシミュレーション　
x <- rhyper(1, m, n, k)	# 超幾何分布に従う乱数１つ抽出　
if(x !=0){
estimate <- c(estimate, k * m / x)
}
} # 個体数推定値列ベクトル
me <- median(estimate)　
o <- c(o, me)
}     #
o
plot(o)
