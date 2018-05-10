#---
#1
#---
antidotes <- read.csv("Antidotes.csv")
antidotes
interaction.plot(antidotes$DOSE, antidotes$ANTIDOTE, antidotes$BLOOD, pch=1:2, type="b")

antidotes$dose <- as.factor(antidotes$DOSE)
antidotes$antidote <- as.factor(antidotes$ANTIDOTE)

model <- lm(BLOOD ~ antidote*dose, data=antidotes)
summary.aov(model)
#解毒剤の種類によって効き目がちがう（主効果ANTIDOTEの有意確率<0.05より）
#投与量によっても効き目がちがう（主効果DOSEの有意確率<0.05より）
#投与量の効果が解毒剤の種類によって異なる（交互作用の有意確率<0.05より）


#---
#2
#---
pitcher <- read.csv("pitcher.csv")
pitcher
interaction.plot(pitcher$opponent, pitcher$time, pitcher$strikeout)
model <- lm(strikeout ~ opponent*time, data=pitcher)
summary.aov(model)
TukeyHSD(aov(model))
#相手チーム（opponent）によって三振数が変わっているので、このピッチャーの得意チームがある（主効果opponentの有意確率<0.05より）
#試合の時間帯によって三振数は変わっていないようである(主効果timeの有意確率>0.05より）
#試合の時間帯によって得意チームが変わるわけではない（交互作用の有意確率>0.05より）

#モデル単純化（授業ではやっていない）
model2 <- lm(strikeout ~ opponent + time, data=pitcher)
summary(model2)
anova(model, model2)

model3 <- lm(strikeout ~ opponent, data=pitcher)
anova(model2, model3)
summary(model3)

