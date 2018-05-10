#----
# 1
#----
#ヤギへの寄生虫駆除処理の効果
#(1)
#共分散分析を行う
#データの読み込み
goats <- read.csv("goats.csv")
str(goats)

plot(Weightgain ~ Initial.wt, data=goats, type="n")
points(Weightgain[Treatment=="intensive"] ~ Initial.wt[Treatment=="intensive"], data=goats)
points(Weightgain[Treatment=="standard"] ~ Initial.wt[Treatment=="standard"], data=goats, pch=16)

#最大モデルをあてはめる
model00 <- lm(Weightgain ~ Initial.wt * Treatment, data=goats)
summary(model00)
#交互作用および、Treatmentのp値が0.05を超えているが、モデル単純化により除去するのは、まず、より高次の項からなので、交互作用項を取り除く
model01 <- update(model00, .~.-Initial.wt:Treatment)
anova(model01, model00)
#説明力に有意な減少が見られないので、model01を採用する。

summary(model01)
#残った説明変数のうち有意でないものはないので、これが最小十分モデルとなる。
#交互作用のあるmodel00ではTreatmentの効果は有意ではなかったが、
#交互作用項を除くとTreatmentの効果が有意となったことに注意。
#これは、model00ではTreatmentによって説明されるべきデータのばらつきが
#交互作用項によっても説明されてしまうため、交互作用項もTreatmentの主効果も有意とならなかったためである。

#(2)
plot(Weightgain ~ Initial.wt, data=goats, type="n")
points(Weightgain[Treatment=="intensive"] ~ Initial.wt[Treatment=="intensive"], data=goats)
points(Weightgain[Treatment=="standard"] ~ Initial.wt[Treatment=="standard"], data=goats, pch=16)
abline(a=14.96661, b=-0.35137)
abline(a=14.96661-1.26486, b=-0.35137, lty=2)

#凡例
legend("topright", legend=c("Intensive", "Standard"), pch=c(1, 16), lty=c(1, 2))



#----
# 2
#----
#クジラ観光データセット
#データの読み込み
ww <- read.csv("whale.watching.csv")
str(ww)

#カテゴリカル型説明変数を因子ベクトルに変換しておく
ww$year <- as.factor(ww$YEAR)
ww$month <- as.factor(ww$MONTH)

#データを図示する
ww2 <- subset(ww, select=c(LRGWHAL, year, month, CLOUD8AM, RAIN8AM, VIS8AM))
#subset関数を用いて、元のデータフレームから一部分を取り出すことができる。
#ここでは、データフレームwwからLRGWHAL, year, month, CLOUD8AM, RAIN8AM, VIS8AMの列を取り出して、
#それをww2というデータフレームに保存している。
pairs(ww2)


#まず最大モデルをあてはめる
#最大モデルとしてすべての説明変数の主効果のみを考慮するモデルとする。（交互作用は考えない）
model00 <- lm(LRGWHAL ~ year + month + CLOUD8AM + RAIN8AM + VIS8AM, data=ww)
summary(model00)
#CLOUD8AMの効果がなさそうなので除去する
model01 <- update(model00, .~.-CLOUD8AM)
anova(model01, model00)
#CLOUD8AMの効果を除去しても説明力に有意なちがいはない。

#CLOUD8AMを除去したモデルのパラメータ推定値を確認
summary(model01)
#RAIN8AMの効果もなさそうなので、除去する。
model02 <- update(model01, .~.-RAIN8AM)
anova(model02, model01)
#RAIN8AMの効果を除去しても説明力に有意なちがいはない。

#RAIN8AMの効果を除去したモデルのパラメータ推定値を確認
summary(model02)
#month5とそれ以外のmonthの間にも有意な違いが見られないので、monthを除去したモデルをあてはめてみる
model03 <- update(model02, .~.-month)
anova(model03, model02)
#しかしmonthの効果を除くと、有意な説明力の減少が見られる。（monthの効果を除くのは単純化しすぎである）
#最小十分モデルはmodel02ということになる。年や月の違いがあるほか、8時における港の視界がクジラの観察頭数の説明変数として残っている。




