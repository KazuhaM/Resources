#二項分布
trial.size <- 10
db <- dbinom(1:trial.size, size=trial.size, prob=0.4)
plot(1:trial.size, db, type="h", lwd=5, cex.lab=1.2, cex.main=1.5,
	main="成功確率0.4、試行回数10の２項分布",
	ylab="確率",
	xlab="成功回数")

#ロジット変換の逆関数（ロジスティックモデル）
inv.logit <- function(x) exp(beta1 + beta2*x)/(1 + exp(beta1 + beta2*x))
beta1 <- 0.2
beta2 <- 0.1
xv <- seq(-60, 60, by=0.1)
plot(xv, inv.logit(xv), lwd=2, cex.lab=1.2, cex.main=1.5,
	main="logit関数の逆関数",
	ylab="p",
	xlab="x")

#
gas.conc <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113,  1.8369, 1.8610, 1.8839)
numb <- c(59, 60, 62, 56, 63, 59, 62, 60)
death <- c(6, 13, 18, 28, 52, 53, 61, 60)

write.csv(data.frame(gas.conc=gas.conc, numb=numb, death=death), "beetle.csv", row.names=F)



#カブトムシの死亡データ作成
#元データ（やや過分散ぎみ）
gas.conc <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113,  1.8369, 1.8610, 1.8839)
numb <- c(59, 60, 62, 56, 63, 59, 62, 60)
death <- c(6, 13, 18, 28, 52, 53, 61, 60)
model01 <- glm(cbind(death, numb-death) ~ gas.conc, binomial) 
summary(model01)

yv <- predict(model01, list(gas.conc=gas.conc), type="response")
deathv <- rbinom(rep(1, length(numb)), numb, yv)
plot(gas.conc, deathv/numb)

model01 <- glm(cbind(deathv, numb-deathv) ~ gas.conc, binomial) #family以下は省略指定可
summary(model01)
deathv
death <- c(5, 9, 34, 37, 52, 54, 60, 60)

#データ
gas.conc <- c(1.6907, 1.7242, 1.7552, 1.7842, 1.8113,  1.8369, 1.8610, 1.8839)
numb <- c(59, 60, 62, 56, 63, 59, 62, 60)
death <- c(5, 9, 34, 37, 52, 54, 60, 60)

#図示
plot(gas.conc, death/numb, pch=16, cex=2,
xlab="ガス濃度", ylab="死亡割合", ylim=c(0, 1))

#GLMのあてはめ
model01 <- glm(cbind(death, numb-death) ~ gas.conc, binomial) 
summary(model01)

#モデル単純化
model02 <- update(model01, .~.-gas.conc)

#モデルの説明力の差の検定（尤度比検定）
anova(model02, model01, test="Chi")
#ガス濃度の効果を除くモデル単純化によって説明力は有意に減少している。

#デビアンス（逸脱度）
deviance(model01)


#推定値を用いたロジスティック曲線を散布図に重ねて描く
plot(gas.conc, death/numb, pch=16, cex=2, xlab="ガス濃度", ylab="死亡割合", ylim=c(0, 1))
#適合値を計算するための説明変数の値を選ぶ
xv <- seq(min(gas.conc), max(gas.conc), by=0.01)
xv
#説明変数の値が与えられたときのモデルの予測値を適合値として（もとの比率の尺度で）計算する
#predict(予測に用いるモデル、名前付きのリストで与える説明変数, type="response")
#type="response"を指定することによって、適合値が得られる。何も指定しないと線形予測子の値が返される。
yv <- predict(model01, list(gas.conc=xv), type="response")
lines(xv, yv, lwd=2) #lwdで線の太さを指定

#問題作成
nisland <- 30
log.area <- runif(nisland, min=3, max=7)

disturbance <- factor(rep(c("high", "low"), c(nisland/2, nisland/2)))
alpha <- 3
beta1 <- -17
beta2 <- -14
p1 <- exp(beta1 + alpha*log.area[1:(nisland/2)])/( 1 + exp(beta1 + alpha*log.area[1:(nisland/2)]) )
p2 <- exp(beta2 + alpha*log.area[(nisland/2 + 1):nisland])/( 1 + exp(beta2 + alpha*log.area[(nisland/2 + 1):nisland]) )
plot(log.area, c(p1, p2))

presence1 <- rbinom(rep(1, length(p1)), rep(1, length(p1)), p1)
presence2 <- rbinom(rep(1, length(p2)), rep(1, length(p2)), p2)
presence <- c(presence1, presence2)
plot(log.area, presence)

model01 <- glm(cbind(presence, 1-presence) ~ log.area * disturbance, family=binomial)
summary(model01)

model02 <- update(model01, .~.-log.area:disturbance)
anova(model02, model01, test="Chi")
summary(model02)

model03 <- update(model02, .~.-disturbance)
anova(model03, model02, test="Chi")
summary(model03)

write.csv(data.frame(log.area, disturbance, presence), file="lizard.csv", row.names=F)

#解答例
lizard <- read.csv("lizard.csv")
str(lizard)

#データの図示
plot(presence ~ log.area, type="n", data=lizard)
#disturbanceが"high"と"low"のデータを両方プロットする。type="n"によって描画を抑えて、描画領域だけを表示する。
points(presence+0.01 ~ log.area, data=subset(lizard, disturbance=="high"), pch=16)
#disturbanceが"high"のデータだけを、subset(lizard, disturbance=="high")によって抽出し、そのデータのpresenceとlog.areaの関係を描く。
#その際、presence+0.01とすることによって、y座標の値を全体的に少し上にあげて、次に描くdisturbanceが"low"のデータと重ならないようにする。
points(presence-0.01 ~ log.area, data=subset(lizard, disturbance=="low"))
#disturbanceが"low"のデータだけを、subset(lizard, disturbance=="low")によって抽出し、そのデータのpresenceとlog.areaの関係を描く。
#presence-0.01とすることによって、y座標の値を全体的に少し上にあげて、次に描くdisturbanceが"high"のデータと重ならないようにする。

#最大モデルのあてはめ
model01 <- glm(cbind(presence, 1-presence) ~ disturbance*log.area, family=binomial)
summary(model01)
#係数推定値の表からdisturbance:log.areaの交互作用項は有意でないので、この交互作用項を除いたモデルをつくる。
model02 <- update(model01, .~.-disturbance:log.area)
#尤度比検定を使って、説明力の差の検定を行う
anova(model02, model01, test="Chi")
#説明力に有意な違いがないので単純な方のモデルを採択する。その係数推定値の表は
summary(model02)
#この表をみるとdisturbanceとlog.areaの両方の影響が認められるため、これ以上のモデルの単純化は必要ない。
#model02を最小十分モデルとする。
#撹乱と島面積の影響はあるが、それらの影響は独立に効いている（交互作用はない）。

#最小十分モデルを図示する。
#まずデータの図示（上と同じ）
plot(presence ~ log.area, type="n", data=lizard, xlab="島面積（㎡、対数）", ylab="トカゲ個体群の存在確率")
points(presence+0.01 ~ log.area, data=subset(lizard, disturbance=="high"), pch=16)
points(presence-0.01 ~ log.area, data=subset(lizard, disturbance=="low"))

#適合値を計算するために、説明変数の値を設定する
log.area.v <- seq(min(lizard$log.area), max(lizard$log.area), by=0.01)

#まずdisturbanceが"high"の予測曲線を描く
yv <- predict(model02, list(log.area=log.area.v, disturbance=rep("high", length(log.area.v))), type="response")
lines(log.area.v, yv)

#つぎにdisturbanceが"low"の予測曲線を描く
yv <- predict(model02, list(log.area=log.area.v, disturbance=rep("low", length(log.area.v))), type="response")
lines(log.area.v, yv, lty=2)

#凡例を描いておく
legend(x=3.2, y=0.9, pch=c(16, 1), lty=c(1, 2), legend=c("高い", "低い"), title="撹乱頻度")
#最初のxとyで凡例の枠の左上の角の位置の座標を指定することができる。


