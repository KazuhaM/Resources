dai <- read.csv("daizu.csv"); dai		#ダイズ地上部乾物重データ読み込み
dim(dai)						#データの大きさ表示
head(dai)			# データの部分表示
height <- as.vector(as.matrix(dai))		#行列データのベクトル化
boxplot(height)					#箱ヒゲ図
boxplot.stats(height)				#箱ひげ図用統計量
#
m <- mean(height, na.rm=TRUE); m		#標本平均
s <- sd(height, na.rm=TRUE); s		#標本標準偏差
op <- par(mfrow = c(1, 2)) 
hist(height, breaks=seq(0,200, by=10), freq=FALSE)
curve(dnorm(x, mean=m, sd=s), 0, 200, add=TRUE, col="red")	#正規分布  
qqnorm(height)			#正規 Q - Q プロット
qqline(height, col="red")
par(op) 



dai <- read.csv("daizu.csv"); dai		#ダイズ地上部乾物重データ読み込み
dai2 <- dai[3:23,3:10]
dai2
dim(dai2)						#データの大きさ表示
head(dai2)			# データの部分表示
height <- as.vector(as.matrix(dai2))		#行列データのベクトル化
boxplot(height)					#箱ヒゲ図
boxplot.stats(height)				#箱ひげ図用統計量
#
m <- mean(height, na.rm=TRUE); m		#標本平均
s <- sd(height, na.rm=TRUE); s		#標本標準偏差
op <- par(mfrow = c(1, 2)) 
hist(height, breaks=seq(0,200, by=10), freq=FALSE)
curve(dnorm(x, mean=m, sd=s), 0, 200, add=TRUE, col="red")	#正規分布  
qqnorm(height)			#正規 Q - Q プロット
qqline(height, col="red")
par(op) 