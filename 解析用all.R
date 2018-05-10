#データ読み込み
y<-c(26,24,19)
hmb<-c(1,2,3)

A<-c("R","C")
B<-c("D","Y","L")
a<-1
b<-3
if (a==1){
	yl<-"Richness"
}else {
	yl<-"All Cover"
}
switch(b,               
   "1" = xl<-"Direction",
   "2" = xl<-"Year",
   "3" = xl<-"Layer"   
)
if (b==2){
	enn<-5
}else{
	enn<-4
}

#Steel.Dwassの式の出力変更
	Steel.Dwass <- function(data,                                                # データベクトル
	                        group)                                          # 群変数ベクトル
	{
	        OK <- complete.cases(data, group)                            # 欠損値を持つケースを除く
	        data <- data[OK]
	        group <- group[OK]
	        n.i <- table(group)                                          # 各群のデータ数
	        ng <- length(n.i)                                            # 群の数
	        t <- combn(ng, 2, function(ij) {
	                i <- ij[1]
	                j <- ij[2]
	                r <- rank(c(data[group == i], data[group == j]))     # 群 i, j をまとめてランク付け
	                R <- sum(r[1:n.i[i]])                                        # 検定統計量
	                N <- n.i[i]+n.i[j]                                   # 二群のデータ数の合計
	                E <- n.i[i]*(N+1)/2                                  # 検定統計量の期待値
	                V <- n.i[i]*n.i[j]/(N*(N-1))*(sum(r^2)-N*(N+1)^2/4)  # 検定統計量の分散
	                return(abs(R-E)/sqrt(V))                                # t 値を返す
	        })
	        p <- ptukey(t*sqrt(2), ng, Inf, lower.tail=FALSE)            # P 値を計算
	        #result <- cbind(t, p)                                                # 結果をまとめる
	        #rownames(result) <- combn(ng, 2, paste, collapse=":")
	        return(p)
	}

SD <- Steel.Dwass(y,hmb)



#種数比較用
#棒グラフの記述
xh<-max(y)*1.2
px <- barplot(y, ylab = yl, xlab = xl, ylim = c(0,xh+1))

enna<-enn-1
if (enn == 5) {
	ha<-NULL
	ennb<-enna-1
	ha<-rbind(c(0,1,2),c(3,4,0),c(5,0,0))
	for (i in 1:ennb){
	ja<-enna-i
	for (j in 1:ja){
		h<-1 *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh -0.8 - h, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh -0.8 - h, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+0.5, labels =paste("***" ,signif(sd,digits=4)) , cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+0.5, labels =paste("**" ,signif(sd,digits=4)), cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+0.5, labels =paste("*" ,signif(sd,digits=4)), cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+0.5, labels =signif(sd,digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0)
		} 	
	}
	}
}else {
	ennb<-enna-1
	ha<-NULL
	ha<-rbind(c(0,1),c(2,0))
	for (i in 1:ennb){
		ja<-enna-i
	for (j in 1:ja){
		h<-0.8 *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh -0.5 - h, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh -0.5 - h, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+0.3, labels =paste("***" ,signif(sd,digits=4)) , cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+0.3, labels =paste("**" ,signif(sd,digits=4)), cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+0.3, labels =paste("*" ,signif(sd,digits=4)), cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+0.3, labels =signif(sd,digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0)
		} 	
	}
	}
}
	
		dev.copy(pdf, file= paste(A[a],"a-",B[b],"5.pdf", sep =""),width = 10, height = 10)
		dev.off()


