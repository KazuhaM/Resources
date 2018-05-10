# 順位－個体数量図
abd <- c(45, 32, 12, 3, 2, 2, 1, 1, 1, 1)
barplot(abd, names.arg = 1:10)

#　オサムシデータ読み込み
osamushi.data <- read.table("osamushi.txt", header=T)
new <- rep(osamushi.data$種,osamushi.data$新しい松林)
new
old <- rep(osamushi.data$種,osamushi.data$古い松林)
old

# ヒストグラム
resamp <- sapply(1:1000, function(x) length( levels( factor( sample(new, 63) ) ) ))
oldpar <- par(no.readonly = TRUE)
par(mar=c(5,5,0,0))
hist(resamp, main="", xlab="種数", ylab="シミュレーション数", col="gray")
par(oldpar)


new

resamp <- sapply(1:1, function(x) length( levels( factor( sample(new, 63) ) ) ))


---
smp <- sample(new, 63)
levels(smp)
lev <- levels(factor(smp))
num <- numeric(length(lev))

for(i in 1:length(lev)){
 num[i] <- length(smp[smp==lev[i]])
}

data.frame(list(lev=lev, num=num))

---




# 希薄化（Rarefaction）
num.resamp <- 10 #　繰り返し数
## Rarefaction for 新しい松林
out <- quantile(1, c(0.025, 0.5, 0.975))
for(i in 2:length(new)){
	resamp <- sapply(1:num.resamp, function(x) length( levels( factor( sample(new, i) ) ) ))
	out <- rbind(out, quantile(resamp, c(0.025, 0.5, 0.975)))
}

## Rarefaction for 古い松林
out2 <- quantile(1, c(0.025, 0.5, 0.975))
for(i in 2:length(old)){
	resamp <- sapply(1:num.resamp, function(x) length( levels( factor( sample(old, i) ) ) ))
	out2 <- rbind(out2, quantile(resamp, c(0.025, 0.5, 0.975)))
}

## 希薄化曲線のPlot
oldpar <- par(no.readonly = TRUE)
par(mar=c(5,5,1,1))
plot(out[,2], type="l", xlab="個体数", ylab="種数", xlim=c(0, 260))
points(out[,1], type="l", lty="dashed")
points(out[,3], type="l", lty="dashed")
points(out2[,2], type="l", col="red")
points(out2[,1], type="l", lty="dashed", col="red")
points(out2[,3], type="l", lty="dashed", col="red")
text(c(70, 250), c(6, 28), labels=c("古い松林", "新しい松林"), col=c("red", "black") )
par(oldpar)

#-------------------------------------
# 漸近種数の推定（Chao1指数）とPIE
#-------------------------------------
# 新しい松林
# Chao1指数
n <- length(new) # 新しい松林からサンプルされた個体数を計算
richness.observed <- length(levels(factor(new))) # 新しい松林からサンプルされた種の数を計算
freq.table <- table(factor(new))   #　度数分布表を作る
singletons <- length(freq.table[freq.table==1])  # 1個体しか出現しなかった種（singleton）を数える
doubletons <- length(freq.table[freq.table==2])  # 2個体しか出現しなかった種（doubleton）を数える
richness.estimated <- richness.observed + ((n - 1) / n) * ( (singletons * (singletons - 1)) / (2*(doubletons + 1)) )  # Chao1を計算する
richness.estimated

#　多様性の指標（均等度も考慮した多様性指標、ＰＩＥ、種間遭遇確率）
total.indiv <- length(factor(new))  # 総個体数を保存
PIE <- ( total.indiv/(total.indiv-1) ) * ( 1 - sum( (freq.table/sum(freq.table))^2 ) )
PIE  # probability of an interspecific encounter （種間遭遇確率）

# 古い松林
# Chao1指数
n <- length(old)
richness.observed <- length(levels(factor(old))) # 薬師からサンプルされた種の数を計算
freq.table <- table(factor(old))   #　度数分布表を作る
singletons <- length(freq.table[freq.table==1])  # 1個体しか出現しなかった種（singleton）を数える
doubletons <- length(freq.table[freq.table==2])  # 2個体しか出現しなかった種（doubleton）を数える
richness.estimated <- richness.observed + ((n - 1) / n) * ( (singletons * (singletons - 1)) / (2*(doubletons + 1)) )  # Chao1を計算する
richness.estimated


#　多様性の指標（均等度も考慮した多様性指標、ＰＩＥ、種間遭遇確率）
total.indiv <- length(factor(furui.yakushi))  # 総個体数を保存
PIE <- ( total.indiv/(total.indiv-1) ) * ( 1 - sum( (freq.table/sum(freq.table))^2 ) )
PIE  # probability of an interspecific encounter （種間遭遇確率）

