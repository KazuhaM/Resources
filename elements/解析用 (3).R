#for (k in 1 : 2){
#for (l in 1 : 3){ 

#データ読み込み
A<-c("C","D","R")
B<-c("d","l","m","y")
a<-1
b<-1
if (a==1){
	yl<-"All Cover"
}else 
if (a == 2){
	yl<-"Index of Diversity"
}else 
if (a == 3){
	yl<-"Richness"
}

switch(b,               
   "1" = xl<-"Direction",
   "2" = xl<-"Layer",
   "3" = xl<-"Mound",
   "4" = xl<-"Year"    
)

flname<-paste(A[a],"-",B[b],".csv", sep ="")

x<-read.csv(flname,header=TRUE)
enn<-ncol(x)
 
x.mean<-apply(x[1:enn-1], 2 , mean , na.rm=T)
x.se<-c()
hm<-c()
sda<-c()
enna<-enn-1
for (i in 1 : enna){
	#if(is.na(names(table(is.na(x[,i])==FALSE)["TRUE"]))){
	#	hm[i]<-0
	#	break
	#	}else
		print(i)
		if(table(is.na(x[,i])==FALSE)["TRUE"]==length(x[,i])){
			hm[i]<-length(x[,i])
		}else {
			hm[i]<-table(is.na(x[,i])==FALSE)["TRUE"]
		}
	sda[i]<-sd(x[,i], na.rm=T)
	hma[i]<-sqrt(hm[i])
	x.se[i]<-sda [i]/ hma[i]
}

x.sd <- apply(x[1:enn-1], 2, sd, na.rm = T)

#Steel.Dwass用配列
y<-x[!is.na(x[,1:enn-1])]
hmb<-x[,enn]
y<-y[1:length(hmb)]


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
		  #return(result)
	}
#Steel.Dwass(y,hmb)
SD <- Steel.Dwass(y,hmb)

#棒グラフの記述
xh<-max(x.mean + x.se)*1.2
px <- barplot(x.mean, ylab = yl, xlab = xl, ylim = c(0,xh+10))
#誤差バーを描く、lengthは誤差バーのバーの長さを示す、angleを鋭角にするとバーは矢印状となる
arrows(px, x.mean - x.se, px, x.mean + x.se, angle = 90, length = 0.1)
arrows(px, x.mean + x.se, px, x.mean - x.se, angle = 90, length = 0.1)

#被度比較用
if (enn == 5) {
	ha<-NULL
	ennb<-enna-1
	ha<-rbind(c(0,1,2),c(3,4,0),c(5,0,0))
	for (i in 1:ennb){
		ja<-enna-i
	for (j in 1:ja){
		h<-5 *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh -3 - h, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh -3 - h, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+2, labels =paste("***" ,signif(sd,digits=4)) , cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+2, labels =paste("**" ,signif(sd,digits=4)), cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+2, labels =paste("*" ,signif(sd,digits=4)), cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+2, labels =signif(sd,digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0)
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
		h<-5 *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh -3 - h, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh -3 - h, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+2, labels =paste("***" ,signif(sd,digits=4)) , cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+2, labels =paste("**" ,signif(sd,digits=4)), cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+2, labels =paste("*" ,signif(sd,digits=4)), cex = 0.8 , font = 1, ps = 8, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+2, labels =signif(sd,digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0)
		} 	
	}
	}
}
	
		dev.copy(pdf, file= paste(A[a],"-",B[b],".pdf", sep =""),width = 10, height = 10)
		dev.off()



#種数比較用
#棒グラフの記述
xh<-max(x.mean + x.se)*1.5
px <- barplot(x.mean, ylab = yl, xlab = xl, ylim = c(0,xh+1))
#誤差バーを描く、lengthは誤差バーのバーの長さを示す、angleを鋭角にするとバーは矢印状となる
arrows(px, x.mean - x.se, px, x.mean + x.se, angle = 90, length = 0.1)
arrows(px, x.mean + x.se, px, x.mean - x.se, angle = 90, length = 0.1)


if (enn == 5) {
	ha<-NULL
	ennb<-enna-1
	ha<-rbind(c(0,1,2),c(3,4,0),c(5,0,0))
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
	
		dev.copy(pdf, file= paste(A[a],"-",B[b],"5.pdf", sep =""),width = 10, height = 10)
		dev.off()


