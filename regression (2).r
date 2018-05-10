#パッケージインストール
pckname<-"segmented"
if(require(pckname)){
    print(paste(pckname," is loaded correctly"))
} else {
    print(paste("trying to install ",pckname,"..."))
    install.packages(pckname)
    if(require(pckname)){
        print(paste(pckname,"installed and loaded"))
    } else {
        stop("could not install")
    }
}
pckname<-"ggplot2"
if(require(pckname)){
    print(paste(pckname," is loaded correctly"))
} else {
    print(paste("trying to install ",pckname,"..."))
    install.packages(pckname)
    if(require(pckname)){
        print(paste(pckname,"installed and loaded"))
    } else {
        stop("could not install")
    }
}
#2016年2017年でデータ分け
data16<-data2[data2$year==2016,]
data16<-data16[order(data16[,4]),]
data17<-data2[data2$year==2017,]
x16<-data16$cover
y16<-data16$count
x17<-data17$cover
y17<-data17$count
xx <-data2$cover
yy <-data2$count
dat <- data.frame(xx, yy)
dat16 <- data.frame(x16, y16)
dat17 <- data.frame(x17, y17)

par(mar = c(5.1, 4.5, 4.1, 2.1))
plot(x16,y16,
	main="Relationship between 
	Drifting Sands and Vegetation Coverage",
	xlab="Vegetation Coverage(%)",ylab="Drifting Sands(n)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
	col=4,pch=1,
	xlim=c(0,35),ylim=c(0,10000)
	)
par(new=T)
plot(x17,y17,
	col=2,pch=4,
	xlim=c(0,35),ylim=c(0,10000),
	ann = F,axes = F
)
legend(locator(1), legend=c("2016","2017"),col=c(4,2),pch=c(1,4))

#piecewise regression
##2016
dati16 <- data.frame(x = x16, y = y16)
out.lm16 <- lm(y ~ x, data = dati16)
o16 <- segmented(out.lm16, seg.Z = ~x
)
dat216 = data.frame(x = x16, y = broken.line(o16)$fit)

##2017
dati17 <- data.frame(x = x17, y = y17)
out.lm17 <- lm(y ~ x, data = dati17)
o17 <- segmented(out.lm17, seg.Z = ~x,
  control = seg.control(display = FALSE)
) 
dat217 = data.frame(x = x17, y = broken.line(o17)$fit)

##all
dati <- data.frame(x = xx, y = yy)
out.lm <- lm(y ~ x, data = dati)
o <- segmented(out.lm, seg.Z = ~x,
  control = seg.control(display = FALSE)
)
dat2 = data.frame(x = xx, y = broken.line(o)$fit)

#プロット
windows()
plot(	main="Relationship between Drifting Sands and Vegetation Coverage",
	xlab="Vegetation Coverage(%)",ylab="Drifting Sands(n)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8     #  メインタイトルの字の大きさを設定する
	)

ggplot(dati16, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat216, color = 'blue')
par(new=T)
ggplot(dati17, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat217, color = 'red')
par(new=T)
ggplot(dati, aes(x = x, y = y)) +
  geom_point() +
  geom_line(data = dat2, color = 'black')
	dev.copy(pdf, file="piecewise.pdf",sep=""), width = 10, height = 10)
	dev.off()



#exponential
resid <- function(par)	# 関数名は何でも良い。引数は，パラメータのベクトル。x, y は，大域変数として参照する。
{
	yhat <- par[1]+par[2]*exp(-par[3]*x16)
	sum((y16-yhat)^2)	# 残差平方和を返す
}
st<-c(1,1,1)
optim(st, resid)
ea16<-list(a=optim(st, resid)$par[1],
	b = optim(st, resid)$par[2],
	c = optim(st, resid)$par[3])
e16.model <- nls(y16 ~ a+b*exp(-c*x16), dat16, start=ea16)

resid <- function(par)	# 関数名は何でも良い。引数は，パラメータのベクトル。x, y は，大域変数として参照する。
{
	yhat <- par[1]+par[2]*exp(-par[3]*x17)
	sum((y17-yhat)^2)	# 残差平方和を返す
}
st<-c(20,20,20)
optim(st, resid)
ea17<-list(a=optim(st, resid)$par[1],
	b = optim(st, resid)$par[2],
	c = optim(st, resid)$par[3])
#ea17<-list(a=100,b = 100,c = 1)
e17.model <- nls(y17 ~ a+b*exp(-c*x17), dat17, start=ea17)

resid <- function(par)	# 関数名は何でも良い。引数は，パラメータのベクトル。x, y は，大域変数として参照する。
{
	yhat <- par[1]+par[2]*exp(-par[3]*xx)
	sum((yy-yhat)^2)	# 残差平方和を返す
}
st<-c(1,1,1)
eax<-list(a=optim(st, resid)$par[1],
	b = optim(st, resid)$par[2],
	c = optim(st, resid)$par[3])
e.model <- nls(yy ~ a+b*exp(-c*xx), dat, start=eax)

predict.e16 <- predict(e16.model)
predict.e17 <- predict(e17.model)
predict.e <- predict(e.model)
 plot(x16, y16, ann=F, xlim=c(0,40), ylim=c(0,10000));   par(new=T)
 plot(x16,predict.e16, type="l", xlim=c(0,40), ylim=c(0,10000))

#inverse
e16.model <- nls(y16 ~ a+b/x16, dat16, start=list(a=1, b=1))
i17.model <- nls(y ~ a+b/x, dat17, start=list(a=1, b=1))
i.model <- nls(y ~ a+b/x, dat, start=list(a=1, b=1))

#logit
l16.model <- nls(y ~ a-b*exp(-exp(c+d*log(x))), dat16, start=list(a=1, b=1, c=1, d=1))
l17.model <- nls(y ~ a-b*exp(-exp(c+d*log(x))), dat17, start=list(a=1, b=1, c=1, d=1))
l.model <- nls(y ~ a-b*exp(-exp(c+d*log(x))), dat, start=list(a=1, b=1, c=1, d=1))




