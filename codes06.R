#データ入力
weight <- c(30, 29, 31, 28, 38, 36, 23, 22, 26, 27, 36, 35)
feed <- as.factor(c(rep(rep(c("A", "B", "C"), times=c(2, 2, 2)), 2)))
sex <- as.factor(c(rep("male", 6), rep("female", 6)))

#交互作用図
interaction.plot(feed, sex, weight)

model <- lm(weight ~ feed + sex + feed:sex)
summary.aov(model)

summary(model)


model0 <- lm(weight ~ 1)
anova(model, model0)

#多重比較TukeyのHSD
TukeyHSD(aov(weight ~ feed + sex + feed:sex))
tukey <- TukeyHSD(aov(model))
plot(tukey)


