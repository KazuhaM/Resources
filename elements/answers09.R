#-----
# 1
#-----
#解析例
lt <- read.csv("lizard.ticks.csv")
str(lt)
#データの傾向を図示
plot(lt$SVL, lt$ticks, type="n")
points(lt$SVL[lt$sex=="female"], lt$ticks[lt$sex=="female"])
points(lt$SVL[lt$sex=="male"], lt$ticks[lt$sex=="male"], pch=16)

#共分散分析の最大モデルをあてはめる
model01 <- glm(ticks ~ sex*SVL, family=poisson, data=lt)
summary(model01)
#交互作用項が有意なので、これ以上単純化はしない。model01が最小十分モデル
#SVLのp値が有意でないので、femaleにおけるSVLの影響はないことがわかる。
#解析の結論としては、「メスでは頭胴長にダニ数は影響されないが、オスでは頭胴長が大きくなるほどダニ数が増えている」ということになる。


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


#-----
# 2
#-----
#解析例
tomatos <- read.csv("tomatos.csv")
str(tomatos)
#データの傾向を図示
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



