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

# 散布図に回帰直線を重ね描きする
plot(tannin, growth)
abline(lm(growth ~ tannin))

# 決定係数
x <- c(1:20)
y1 <- x + rnorm(20, mean=0, sd=5)
y2 <- x + rnorm(20, mean=0, sd=1)
plot(x,y1, ylim=c(min(y1,y2),max(y1,y2)), pch=16)
abline(0,1)
cor(x,y1)^2
plot(x,y2, ylim=c(min(y1,y2),max(y1,y2)), pch=16)
abline(0,1)
cor(x,y2)^2







