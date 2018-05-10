#重回帰
fox <- read.csv("urban.foxes.csv")
fox
str(fox)

pairs(fox)

model00 <- lm(WEIGHT ~ AVFOOD + GSIZE + AREA, data=fox)
summary(model00)
model01 <- update(model00, .~. - AREA)
anova(model01, model00)
summary(model01)
model01

#共分散分析
#子羊の脂肪
#この例も交互作用が有意にならず、交互作用を除くとKindの効果が現れるので、モデル単純化の例題として良い。
lambs <- read.csv("lambs.csv")
str(lambs)

boxplot(Fatness ~ Kind, data=lambs)
t.test(Fatness ~ Kind, data=lambs)

oldpar <- par(no.readonly=TRUE)
par(cex=2)
plot(Fatness ~ Carcasse.wt, data=lambs, type="n", xlab="体重", ylab="脂肪量")
points(Fatness[Kind=="Ewe"] ~ Carcasse.wt[Kind=="Ewe"], data=lambs)
points(Fatness[Kind=="Ram"] ~ Carcasse.wt[Kind=="Ram"], data=lambs, pch=16)
legend("topleft", pch=c(1, 16), legend=c("雌羊","雄羊"))
par(oldpar)


plot(Fatness ~ Carcasse.wt, data=lambs, type="n", xlab="体重", ylab="脂肪量")
points(Fatness[Kind=="Ewe"] ~ Carcasse.wt[Kind=="Ewe"], data=lambs)
points(Fatness[Kind=="Ram"] ~ Carcasse.wt[Kind=="Ram"], data=lambs, pch=16)
legend("topleft", pch=c(1, 16), legend=c("雌羊","雄羊"))
abline(lm(Fatness[Kind=="Ewe"] ~ Carcasse.wt[Kind=="Ewe"], data=lambs))
abline(lm(Fatness[Kind=="Ram"] ~ Carcasse.wt[Kind=="Ram"], data=lambs), lty=2)

#モデル単純化
model00 <- lm(Fatness ~ Kind * Carcasse.wt, data=lambs)
summary(model00)
model01 <- update(model00, .~.-Kind:Carcasse.wt)
anova(model01, model00)

summary(model01)

#最小十分モデルの図示
plot(Fatness ~ Carcasse.wt, data=lambs, type="n", xlab="体重", ylab="脂肪量")
points(Fatness[Kind=="Ewe"] ~ Carcasse.wt[Kind=="Ewe"], data=lambs)
points(Fatness[Kind=="Ram"] ~ Carcasse.wt[Kind=="Ram"], data=lambs, pch=16)
legend("topleft", pch=c(1, 16), legend=c("雌羊","雄羊"))
abline(a=-5.8609, b=1.1275)#雌羊の回帰直線、切片が-5.8609、傾きが1.1275
abline(a=-5.8609 - 4.8804, b=1.1275, lty=2) #雄羊の回帰直線、切片が-5.8609-4.8804、傾きが1.1275






