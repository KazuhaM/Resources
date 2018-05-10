#---
# 3
#---
lizard <- read.csv("lizard.csv")
str(lizard)

#データの図示
plot(presence ~ log.area, type="n", data=lizard, xlab="島面積（㎡、対数）", ylab="トカゲ個体群の存在確率")
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


