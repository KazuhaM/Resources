#相関関係
#可視化
#データ入力
homerun <- c(32, 55, 36, 5, 33, 4, 21, 9, 42, 25)
deadball <- c(76, 100, 53, 47, 27, 25, 52, 52, 86, 30)

#散布図を描く
plot(homerun, deadball)
#点を黒色の丸印にし、ｘ軸とｙ軸にラベルをつける
plot(homerun, deadball, pch=16, xlab="本塁打数", ylab="四球数")

#相関係数を計算
cor(homerun, deadball)

#相関係数の検定
cor.test(homerun, deadball, alternative="greater")

#適合度検定
endo <- c(315, 108, 101, 32) #観察度数
chisq.test(endo, p=c(9/16, 3/16, 3/16, 1/16))
#カテゴリ比率は足して１になっていることに注意

#独立性の検定
#データを行列にまとめる。
bloodtype <- matrix(c(57, 33, 46, 14, 
		      89, 24, 75, 12), byrow=T, ncol=2)

#chisq.testを適用
chisq.test(bloodtype)
chisq.test(matrix(c(1, 2, 3, 4), byrow=T, ncol=2))

#フィッシャーの正確確率検定
#データ入力
finger <- matrix(c(5, 1, 2, 8), byrow=T, ncol=2)
chisq.test(finger)$expected
fisher.test(finger, alternative="greater")
fisher.test(finger, alt="g") #alternativeのオプションを省略して指定できる
(5/1)/(2/8)
fisher.test(bloodtype)


