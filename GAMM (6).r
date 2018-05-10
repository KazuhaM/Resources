#パッケ－ジ
library(mgcv)
library(ggplot2)
library(GGally)
library(MuMIn)

#データ読み込み
flname<-"SumCover"
e.data <- read.csv(paste(flname,".csv",sep=""),header = T)
head(e.data)

plot(e.data$ErosionPin~e.data$Cover)

#ggpairs(na.omit(e.data ), 
#        upper=list(continuous="smooth"),
#        params=list(corSize=6,labelSize=10)) #要素間の総当たりグラフを描く#

#GAMM
gam.model <- gam(ErosionPin~s(Cover), data=e.data,random =ID)    
summary(gam.model)
#GLM
lm.model <- gam(ErosionPin~Cover, data=e.data,random =ID)
summary(lm.model)

#GAMMをプロット
plot(gam.model, residuals=T, pch=1, se=T, main="Spline smoothing non Annual herbs",xlab="Cover(%)",ylab="Erosion Pin(cm)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8      #  メインタイトルの字の大きさを設定する
)
	dev.copy(pdf, file=paste(flname,".pdf",sep=""), width = 10, height = 10)
	dev.off()
#GAMMが当てはまりすぎていないかチェック
gam.check(gam.model)

	dev.copy(pdf, file=paste(flname,"_Check.pdf",sep=""), width = 10, height = 10)
	dev.off()


# lm,GAMM両方の図を作成
en.data <- data.frame(Cover=seq(min(e.data$Cover), max(e.data$Cover), 0.1))
gam.pred <- predict(gam.model, en.data)
lm.pred <- predict(lm.model, en.data)

plot(
  e.data$ErosionPin ~ e.data$Cover,
  xlab="SumCover　not Annual", ylab="ErosionPin(mm)", 
  main="Spline smoothing by GAMM ", cex.main=2, cex.lab=1.5
)
lines(lm.pred ~ as.matrix(en.data), col=2, lwd=2)
lines(gam.pred ~ as.matrix(en.data), col=2, lwd=2)
legend("topleft", lwd=2, col=c(2, 4), legend=c("線形 回帰", "平滑化スプライン"))

#lmより、GAMMのほうがいいこと確かめ
anova(lm.model, gam.model,test="F")

#GAMMの見やすい図
en.data <- data.frame(Cover=seq(min(e.data$Cover), max(e.data$Cover), 0.1))
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
  geom_point(aes(x=Cover,y=ErosionPin), size=5) +
  geom_line(data=gam.pred, 
            aes(x=Cover, y=fit), size=1.2, colour="red")+
  geom_ribbon(data=gam.pred, 
              aes(x=Cover, ymin=conf.lwr, ymax=conf.upr),
              alpha=0.2, fill="red")+
  labs(x = "Cover", 
       y = "ErosionPin", size=2) +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))


# グネグネ度（平滑化パラメータ）の確認
# spを0.001から0.1まで、0.001刻みで変化させる
sp <- seq(from=0, to=10, by=0.01)
 
# spごとに、GCVを計算
GCV <- numeric()
for(i in 1:length(sp)){
  g.m <- gam(ErosionPin~s(Cover), sp=sp[i], data=e.data)
  GCV[i] <- g.m$gcv.ubre
}
 
# spとGCVの関係をプロット
plot(GCV ~ sp, main="GCVとグネグネ度", cex.main=2)
 
# mgcv関数で選ばれたspを重ねてプロットする
points(gam.model$sp, gam.model$gcv.ubre, col=2, pch=18, cex=2)

