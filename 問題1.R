x <- 0:12	 #個数　
gchild <- c(7,45,181,478,829,1112,1343,1033,670,286,104,24,3)	 #回数データ　
sum(gchild)	 #試行回数　
pgchild <- gchild/sum(gchild)	 #回数の確率　
m <- sum(x*pgchild); m	 #平均　
p <- m/12	 #5，6の出る確率　
s2 <- sum(pgchild*(x-m)^2); s2	 #分散　
v <- 12*p*(1-p)	 #二項分布のもとでの分散　
h1 <- dbinom(x, 12, 1/2)	 #正しいサイコロのもとでの二項確率分布　
h2 <- dbinom(x, 12, p)	 #推定確率からの二項確率分布　
gchilddis <- rbind(pgchild,h2,h1)	 #行ベクトル－＞行列　
colnames(gchilddis) <- as.character(0:12)	 #列の名前　
barplot(gchilddis, beside=TRUE, cex.axis=0.8, cex.lab=1.0, xlab="Number of girls", ylab="Probability", legend=c("data","p=0.48", "p=0.5"))
title(main="Distribution of the experiment of girls ")	 #グラフタイトル　




