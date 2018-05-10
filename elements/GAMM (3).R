#パッケ−ジ
library(mgcv)
library(ggplot2)
library(GGally)
library(MuMIn)

#データ読み込み
e.data <- read.csv("EC_Covermover4.csv",header = T)
#e.data
head(e.data)
plot(e.data$Num~e.data$SumCover)

#ggpairs(na.omit(e.data ), 
#        upper=list(continuous="smooth"),
#        params=list(corSize=6,labelSize=10)) #要素間の総当たりグラフを描く#

#GAMM
gam.model <- gam(Num~s(SumCover), data=e.data,random =ID )    
summary(gam.model)
#GLM
lm.model <- gam(Num~SumCover, data=e.data,random =QuadratID)
summary(lm.model)
plot(e.data[,3],e.data[,2],xlim=c(9,31),ylim=c(-500,10000),xlab="Cover",ylab="Wind blow sand")
colo<-rainbow(10)
for (i in 1 : length(e.data[,2])){
	switch(e.data[i,4],
	"CTE"= points(e.data[i,3],gam.model$y[i],col=colo[1]),
	"CTW"= points(e.data[i,3],gam.model$y[i],col=colo[2]),
	"13E"= points(e.data[i,3],gam.model$y[i],col=colo[4]),
	"13F"= points(e.data[i,3],gam.model$y[i],col=colo[3]),
	"13H"= points(e.data[i,3],gam.model$y[i],col=colo[6]),
	"13I"= points(e.data[i,3],gam.model$y[i],col=colo[5]),
	"14B"= points(e.data[i,3],gam.model$y[i],col=colo[8]),
	"14C"= points(e.data[i,3],gam.model$y[i],col=colo[7]),
	"14E"= points(e.data[i,3],gam.model$y[i],col=colo[10]),
	"14F"= points(e.data[i,3],gam.model$y[i],col=colo[9])	
	)
}
legend(locator(1),c("CTE","CTE","13-1E","13-1W","13-2E","13-2W","14-1E","14-1W","14-2E","14-2W"),
	pch=1,col=colo)
plot(gam.model,add=T,residuals=T)

#GAMMをプロット
plot(gam.model, residuals=T, pch="。", se=T, main="Spline smoothing wond speed over 4",
	ylab="Drafting Sand(n)")

#GAMMが当てはまりすぎていないかチェック
gam.check(gam.model)


# lm,GAMM両方の図を作成
en.data <- data.frame(SumCover=seq(min(e.data$SumCover), max(e.data$SumCover), 0.1))
gam.pred <- predict(gam.model, en.data)
lm.pred <- predict(lm.model, en.data)

plot(
  e.data$Num ~ e.data$SumCover,
  xlab="SumCover", ylab="Num", 
  main="", cex.main=2, cex.lab=1.5
)
lines(lm.pred ~ as.matrix(en.data), col=2, lwd=2)
lines(gam.pred ~ as.matrix(en.data), col=4, lwd=2)
legend("topleft", lwd=2, col=c(2, 4), legend=c("線形 回帰", "平滑化スプライン"))

#lmより、GAMMのほうがいいこと確かめ
anova(lm.model, gam.model, test="F")

#GAMMの見やすい図
en.data <- data.frame(SumCover=seq(min(e.data$SumCover), max(e.data$SumCover), 0.1))
gam.pred <- predict(gam.model, en.data, se.fit=TRUE, type="response")
lm.pred <- predict(lm.model, en.data, se.fit=TRUE, type="response")
critval=qnorm(0.975,0,1)
conf.lwr <- gam.pred$fit - critval* gam.pred$se.fit
conf.upr <- gam.pred$fit + critval* gam.pred$se.fit
gam.pred <- data.frame(en.data, 
                       as.data.frame(gam.pred), 
                       conf.lwr = conf.lwr, 
                       conf.upr = conf.upr )

ggplot(data=e.data) +
  geom_point(aes(x=SumCover,y=Num), size=5) +
  geom_line(data=gam.pred, 
            aes(x=SumCover, y=fit), size=1.2, colour="red")+
  geom_ribbon(data=gam.pred, 
              aes(x=SumCover, ymin=conf.lwr, ymax=conf.upr),
              alpha=0.2, fill="red")+
  labs(x = "SumCover", 
       y = "Num", size=2) +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))


# グネグネ度（平滑化パラメータ）の確認
# spを0.001から0.1まで、0.001刻みで変化させる
sp <- seq(from=0, to=10, by=0.01)
 
# spごとに、GCVを計算
GCV <- numeric()
for(i in 1:length(sp)){
  g.m <- gam(Num~s(SumCover), sp=sp[i], data=e.data)
  GCV[i] <- g.m$gcv.ubre
}
 
# spとGCVの関係をプロット
plot(GCV ~ sp, main="GCVとグネグネ度", cex.main=2)
 
# mgcv関数で選ばれたspを重ねてプロットする
points(gam.model$sp, gam.model$gcv.ubre, col=2, pch=18, cex=2)

