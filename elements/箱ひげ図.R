
# データを読み込む
x <- read.csv("D-c.csv", header = TRUE)
y <- x[, -1]                         # データを取り出して y に保存する
type <- unique(x[, 1])               # 種を取得
name<-colnames(x)
name[1]
en<-substring(name[1],1,1)
el<-substring(name[1],3,3)
switch(en,               # switch(文字列,
   "D" = xl<-"Direction",
   "Y" = xl<-"Year",
   "L" = xl<-"Layer"   
)
switch(el,               # switch(文字列,
   "c" = yl<-"cover",
   "h" = yl<-"hight",
   "n" = yl<-"number",
   "f" = yl<-"flower"   
)

par(oma = c(0, 0, 4, 0)) #余白の設定
par(mfcol=c(4,5)) #ウィンドウの分割

	switch(n,			#ウィンドウの分割
		"13" = par(mfcol=c(3,5)):print("1"),
		"19" = par(mfcol=c(4,5)):print("2"),
		print("EROOR")
		)
# 各項目それぞれについてボックスプロットを描く
for (i in 2:length(type)-1) {
	k=3*i-2
	l=3*i
	plot(0, 0, type = "n",  xlim = range(1:3),
		ylim = range(max(y[,k:l],na.rm = TRUE):min(y[,k:l],na.rm = TRUE)) , xlab = xl, 
			ylab = yl, axes = FALSE)
	boxplot(y[,3*i-2],y[,3*i-1],y[,3*i],
      	col = "white" , xaxt = "n", add = TRUE, boxwex = 0.3, type = "n",
			main=type[i], xlab = "Layer", ylab = "number")
	axis(1, at = 1:3, labels = name[2:4], tick = TRUE)
}

######################ここまで#############################

		mx<-max(y[,16:18],na.rm = TRUE)
		mx
		mn<-min(y[,16:18],na.rm = TRUE)
		mn
		range(mn:mx)
		ylim = range( mn : mx ) 
		ylim


par(family = "HiraKakuProN-W3")  # 日本語表示のため
mids <- barplot(mean.group, beside = T, col = c("gray", 
    "white"), las = 1, legend = c("普通教室", "CALL教室"), 
    ylim = c(0, 100), xlab = "クラスサイズ", ylab = "点数", 
    names = c("10人クラス", "20人クラス", "40人クラス"), 
    main = "Mean and 95% CI", yaxp = c(0, 100, 10))
arrows(mids, mean.group - ci.group, mids, mean.group + 
    ci.group, code = 3, angle = 90, length = 0.1)
text(mids, 3, paste("n =", n.group))

# 線を加える
segments(x0 = 1.5, y0 = 80, x1 = 1.5, y1 = 83)  #xとyの数値で始点と終点を指定
segments(x0 = 1.5, y0 = 83, x1 = 2.5, y1 = 83)
segments(x0 = 2.5, y0 = 83, x1 = 2.5, y1 = 70)

segments(x0 = 4.5, y0 = 70, x1 = 4.5, y1 = 73)  #xとyの数値で始点と終点を指定
segments(x0 = 4.5, y0 = 73, x1 = 5.5, y1 = 73)
segments(x0 = 5.5, y0 = 70, x1 = 5.5, y1 = 73)

segments(x0 = 7.5, y0 = 51, x1 = 7.5, y1 = 68)  #xとyの数値で始点と終点を指定
segments(x0 = 7.5, y0 = 68, x1 = 8.5, y1 = 68)
segments(x0 = 8.5, y0 = 68, x1 = 8.5, y1 = 64)

# アステリスクを加える
text(x = 2, y = 85, labels = "***", family = "mono", 
    font = 1, ps = 8)
text(x = 8, y = 70, labels = "***", family = "mono", 
    font = 1, ps = 8)

 plot(1:10)
		dev.copy(pdf, file=paste( en,"-", el, ".pdf"))
		dev.off()                                         # 必要な出力がすべて終ったらすぐにデバイスを閉じる
                                                   # または，前述の作図デバイスで同じことをやると ...
 pdf()                                             # pdf デバイスを開く()
 plot(1:10)                                        # プロット → Rplots.pdf に出力
 dev.off()                                         # 必要な出力がすべて終ったらすぐにデバイスを閉じる


#色の読み込み
#library(RColorBrewer) 
#cols <- brewer.pal(18,"Set1")    #色の取得 種に応じて色を塗り分ける


for (i in 1:length(type)) {
  k=3*i-2
  l=3*i
　　plot(0, 0, type = "n",  xlim = range(1:3),
	ylim = range(max(y[,k:l],na.rm = TRUE):min(y[,k:l],na.rm = TRUE)) , xlab = "Layer", 
		ylab = "number", axes = FALSE)
  boxplot(y[,3i-2],y[,3i-1],y[,3i],
          col = "white" , xaxt = "n", add = TRUE, boxwex = 0.3, type = "n",
			main=type[i] )
	#legend("topleft", legend = type[i], col = cols[i] , pch = 15, 
	#	bg = "transparent")
  # x 軸を書き入れる
  #axis(1, at = 1:3, labels = colnames(y), tick = TRUE)
}

# 何もない図を作成（座標軸は後で描き入れる）
plot(0, 0, type = "n", xlim = range(1:ncol(y)), ylim = range(x[, -1]),xlab = "Layer", ylab = "number", axes = FALSE)
#plot(0, 0, type = "n", xlim = range(1:ncol(y)), ylim =range(0:20),
#	xlab = "Layer", ylab = "number", axes = FALSE)

# 各項目それぞれについてボックスプロットを描く
for (i in 1:length(type)) {
  boxplot(y[x[, 1] == type[i], ],
          col = cols[i], xaxt = "n", add = TRUE)
}

# 凡例
legend("topleft", legend = type, col = cols, pch = 15)

# x 軸を書き入れる
axis(1, at = 1:ncol(y), labels = colnames(y), tick = TRUE)