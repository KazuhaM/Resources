#対数変換の逆関数
beta1 <- 0.2
beta2 <- 0.01
xv <- seq(-60, 60, by=0.1)
plot(xv, exp(beta1 + beta2*xv), lwd=2, cex.lab=1.2, cex.main=1.5,
	main="指数関数（対数関数の逆関数）",
	ylab="lambda",
	xlab="x")


#ライチョウ（例題作成）

beta <- (log(9) - log(4))/(650 - 550)
alpha <- beta*(-550) + log(4)
exp(alpha + beta*600)

nsamp <- 10
weight <- round(runif(nsamp, min=550, max=650)); weight
eggs <- sapply(weight, function(weight) rpois(1, lambda=exp(alpha + beta*weight)) );eggs

plot(weight, eggs)
summary(glm(eggs ~ weight, family=poisson))

#ライチョウ
weight <- c(575, 612, 591, 557, 646, 581, 590, 589, 564, 619)
eggs <- c(5, 7, 5, 2, 8, 6, 2, 2, 4, 8)
plot(weight, eggs, xlab="体重（g）", ylab="一腹卵数", cex=2, pch=16)
model01 <- glm(eggs ~ weight, family=poisson)
summary(model01)

#図示
#散布図を描く
plot(weight, eggs, xlab="体重（g）", ylab="一腹卵数", cex=2, pch=16)
#適合値を計算するための説明変数の値を設定
xv <- seq(min(weight), max(weight), by=0.1)
#応答変数の予測値を計算する
yv <- predict(model01, list(weight=xv), type="response")
#xvベクトルをx座標に、yvベクトルをy座標とする点を結ぶ線を描く
lines(xv, yv)

#アリ種数（例題作成）
n.groups <- 2
n.samples <- 20
n.all <- n.groups * n.samples
x <- rep(1:n.groups, rep(n.samples, n.groups))
dist.class <- factor(x, labels=c("near", "far"))
log.area <- round(runif(n.all, min=2, max=8), 1)
Xmat <- model.matrix( ~ dist.class*log.area)
Xmat

beta.v <- c(0.1, 0.2, 0.1, 0)
lin.pred <- Xmat[,]%*%beta.v
lambda <- exp(lin.pred)
richness <- rpois(n=n.all, lambda=lambda)

summary(glm(richness ~ dist.class*log.area, family=poisson))
summary(glm(richness ~ dist.class + log.area, family=poisson))

plot(log.area, richness, type="n")
points(log.area[dist.class=="near"], richness[dist.class=="near"])
points(log.area[dist.class=="near"], richness[dist.class=="near"], pch=16)

write.csv(data.frame(richness=richness, dist.class=dist.class, log.area=log.area), "ants.csv", row.names=F)

#例題の解析例
ants <- read.csv("ants.csv")
str(ants)
plot(ants$log.area, ants$richness, type="n")
points(ants$log.area[ants$dist.class=="near"], ants$richness[ants$dist.class=="near"])
points(ants$log.area[ants$dist.class=="far"], ants$richness[ants$dist.class=="far"], pch=16)

model01 <- glm(richness ~ dist.class*log.area, family=poisson, data=ants)
summary(model01)
model02 <- update(model01, .~.-dist.class:log.area)
anova(model02, model01, test="Chi")
summary(model02)

#predict関数の使い方
predict(model02, list(dist.class=as.factor("near"), log.area=3), type="response")

predict(model02, list(dist.class=as.factor(c("near", "far")), log.area=c(3, 7)), type="response")

図示
#散布図を描く
plot(ants$log.area, ants$richness, type="n", xlab="島面積（㎡, 対数）", ylab="アリ種数")
points(ants$log.area[ants$dist.class=="near"], ants$richness[ants$dist.class=="near"])
points(ants$log.area[ants$dist.class=="far"], ants$richness[ants$dist.class=="far"], pch=16)
log.area.v <- seq(min(ants$log.area), max(ants$log.area), by=0.1)
#まず近い島の回帰曲線を描く
near.v <- as.factor(rep("near", length(log.area.v)))
richness.near.v <- predict(model02, list(dist.class=near.v, log.area=log.area.v), type="response")
lines(log.area.v, richness.near.v, lty=2)
#つぎに遠い島の回帰曲線
far.v <- as.factor(rep("far", length(log.area.v)))
richness.far.v <- predict(model02, list(dist.class=far.v, log.area=log.area.v), type="response")
lines(log.area.v, richness.far.v)
#凡例を追加する
legend("topleft", legend=c("近い島", "遠い島"), pch=c(1, 16), lty=c(2, 1))

#--------
# 練習問題作成
#--------
#トカゲにつくダニの数
n.sex <- 2
n.samples <- 15
n.all <- n.sex * n.samples
x <- rep(1:n.sex, rep(n.samples, n.sex))
sex <- factor(x, labels=c("female", "male"))
SVL <- round(c(runif(n.samples, min=30, max=55), runif(n.samples, min=30, max=50)), 1)
Xmat <- model.matrix( ~ sex*SVL)
Xmat

beta.v <- c(0.1, 0.1, 0.01, 0.05)
lin.pred <- Xmat[,]%*%beta.v
lambda <- exp(lin.pred)
ticks <- rpois(n=n.all, lambda=lambda)
summary(glm(ticks ~ sex*SVL, poisson))
plot(SVL, ticks)

write.csv(data.frame(ticks=ticks, sex=sex, SVL=SVL), "lizard.ticks.csv", row.names=F)

#解析例
lt <- read.csv("lizard.ticks.csv")
str(lt)
plot(lt$SVL, lt$ticks, type="n")
points(lt$SVL[lt$sex=="female"], lt$ticks[lt$sex=="female"])
points(lt$SVL[lt$sex=="male"], lt$ticks[lt$sex=="male"], pch=16)

#共分散分析の最大モデルをあてはめる
model01 <- glm(ticks ~ sex*SVL, family=poisson, data=lt)
summary(model01)
#交互作用項が有意なので、これ以上単純化はしない。model01が最小十分モデル
#SVLのp値が有意でないので、femaleにおけるSVLの影響はないことがわかる。
#解析の結論としては、「メスでは頭胴長にダニ数は影響されないが、オスでは頭胴長が大きくなるほどダニ数が増えている」ということになる。

model10 <- glm(ticks ~ SVL, data=lt, family=poisson)
model11 <- glm(lt$ticks ~ lt$SVL, family=poisson)
summary(model11)

predict(model10, df, type="response")

predict(model11, df, type="response")

str(model10)
str(model11)

list("lt$SVL"=c(40, 45))
df <- data.frame(c(40, 45))
colnames(df) <- as.character("lt$SVL")
df

#図示
plot(lt$SVL, lt$ticks, type="n", xlab="頭胴長", ylab="ダニ数")
points(lt$SVL[lt$sex=="female"], lt$ticks[lt$sex=="female"])
points(lt$SVL[lt$sex=="male"], lt$ticks[lt$sex=="male"], pch=16)

SVL.v <- seq(min(lt$SVL), max(lt$SVL), by=0.1)
#まずメスから
female.v <- as.factor(rep("female", length(SVL.v)))
ticks.female.v <- predict(model01, list(sex=female.v, SVL=SVL.v), type="response")
lines(SVL.v, ticks.female.v, lty=2)
#つぎにオス
male.v <- as.factor(rep("male", length(SVL.v)))
ticks.male.v <- predict(model01, list(sex=male.v, SVL=SVL.v), type="response")
lines(SVL.v, ticks.male.v)
#凡例を追加
legend("topleft", legend=c("メス", "オス"), pch=c(1, 16), lty=c(2, 1))

#トマトの収穫数
n.farmer <- 2
n.samples <- 20
n.all <- n.farmer * n.samples
x <- rep(1:n.farmer, rep(n.samples, n.farmer))
farmer <- factor(x, labels=c("A", "B"))
bugs <- round(c(runif(n.samples, min=5, max=12), runif(n.samples, min=0, max=10)), 0)
Xmat <- model.matrix( ~ farmer*bugs)
Xmat

beta.v <- c(2, 0.1, -0.1, 0)
lin.pred <- Xmat[,]%*%beta.v
lambda <- exp(lin.pred)
tomatos <- rpois(n=n.all, lambda=lambda)
plot(bugs, tomatos)
points(bugs[farmer=="A"], tomatos[farmer=="A"])
points(bugs[farmer=="B"], tomatos[farmer=="B"], pch=16)
summary(glm(tomatos ~ farmer*bugs, poisson))
summary(glm(tomatos ~ farmer+bugs, poisson))
summary(glm(tomatos ~ bugs, poisson))

write.csv(data.frame(fruits=tomatos, farmer=farmer, bugs=bugs), "tomatos.csv", row.names=F)

#解析例
tomatos <- read.csv("tomatos.csv")
#図示
plot(tomatos$bugs, tomatos$fruits, type="n")
points(tomatos$bugs[tomatos$farmer=="A"], tomatos$fruits[tomatos$farmer=="A"])
points(tomatos$bugs[tomatos$farmer=="B"], tomatos$fruits[tomatos$farmer=="B"], pch=16)

#最大モデルのあてはめ
model01 <- glm(fruits ~ farmer*bugs, family=poisson, data=tomatos)
summary(model01)
#交互作用項が有意でないので除く
model02 <- update(model01, .~.-farmer:bugs)
anova(model02, model01, test="Chi")
#説明力に有意な差はないので、model02を採択する。
summary(model02)
#farmerの影響がないので、除く
model03 <- update(model02, .~.-farmer)
anova(model03, model02, test="Chi")
#説明力に有意な差はないので、model03を採択
summary(model03)
#これ以上の単純化はできない。
#どちらが育てたかによってトマトの実がなる数に差はないが、ついた虫の数が多いほど、トマトの数は減っている。

#図示
plot(tomatos$bugs, tomatos$fruits, type="n", xlab="害虫数", ylab="トマト収穫数")
points(tomatos$bugs[tomatos$farmer=="A"], tomatos$fruits[tomatos$farmer=="A"])
points(tomatos$bugs[tomatos$farmer=="B"], tomatos$fruits[tomatos$farmer=="B"], pch=16)

bugs.v <- seq(min(tomatos$bugs), max(tomatos$bugs), by=0.1)
#まずＡさんとＢさんの間で違いはないので、１つの直線を描けばよい。
fruits.v <- predict(model03, list(bugs=bugs.v), type="response")
lines(bugs.v, fruits.v)
#凡例を追加
legend("topright", legend=c("Ａさん", "Ｂさん"), pch=c(1, 16))















