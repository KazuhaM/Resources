s <- read.csv("Elymus.csv",header = T)
t <- read.csv("Caragana.csv",header = T)


enns<-ncol(s)
ennt<-ncol(t)


 
s.mean<-apply(s[1:enns-1], 2 , mean , na.rm=T)
t.mean<-apply(t[1:ennt-1], 2 , mean , na.rm=T)

s.se<-c()
hms<-c()
sdas<-c()
hmas<-c()
ennas<-enns-1
t.se<-c()
hmt<-c()
sdat<-c()
hmat<-c()
ennat<-ennt-1
for (i in 1 : ennas){
	#if(is.na(names(table(is.na(s[,i])==FALSE)["TRUE"]))){
	#	hms[i]<-0
	#	break
	#	}else
		print(i)
		if(table(is.na(s[,i])==FALSE)["TRUE"]==length(s[,i])){
			hms[i]<-length(s[,i])
		}else {
			hms[i]<-table(is.na(s[,i])==FALSE)["TRUE"]
		}
	sdas[i]<-sd(s[,i], na.rm=T)
	hmas[i]<-sqrt(hms[i])
	s.se[i]<-sdas [i]/ hmas[i]
}
s.sd <- apply(t[1:ennt-1], 2, sd, na.rm = T)
for (i in 1 : ennat){
	#if(is.na(names(table(is.na(t[,i])==FALSE)["TRUE"]))){
	#	hmt[i]<-0
	#	break
	#	}else
		print(i)
		if(table(is.na(t[,i])==FALSE)["TRUE"]==length(t[,i])){
			hmt[i]<-length(t[,i])
		}else {
			hmt[i]<-table(is.na(t[,i])==FALSE)["TRUE"]
		}
	sdat[i]<-sd(t[,i], na.rm=T)
	hmat[i]<-sqrt(hmt[i])
	t.se[i]<-sdat [i]/ hmat[i]
}
t.sd <- apply(t[1:enns-1], 2, sd, na.rm = T)

st <- rbind(s.mean,t.mean)
st<-t(st)
matplot(st,pch = 1:2, type = "o",main = "the Changes of Cover 
 Elymus spp. and C. microphylla",
	xlab="Treatment・Year",ylab="Cover(%)",ylim=c(0,11))
e<-c("Elymus spp.","C. microphylla")
c<-c("black","red")
legend(1,6, e,col=1:2,pch=1:2,ncol=1,cex=1,pt.bg="red")
for (i in 1 : 3){
	arrows(i, s.mean[i] - s.se[i], i, s.mean[i] + s.se[i], angle = 90, length = 0.1)	
	arrows(i, s.mean[i] + s.se[i], i, s.mean[i] - s.se[i], angle = 90, length = 0.1)
	arrows(i, t.mean[i] - t.se[i], i, t.mean[i] + t.se[i], angle = 90, length = 0.1,col = "red")	
	arrows(i, t.mean[i] + t.se[i], i, t.mean[i] - t.se[i], angle = 90, length = 0.1,col = "red")

}


		dev.copy(pdf, file=paste(flname,"a.pdf",sep=""), width = 10, height = 10)
		dev.off()

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
xh<-max(x.mean + x.se)*ms
px <- barplot(x.mean, ylab = yl, xlab = xl, ylim = c(0,xh+mxs), ps = ft)
#誤差バーを描く、lengthは誤差バーのバーの長さを示す、angleを鋭角にするとバーは矢印状となる
arrows(px, x.mean - x.se, px, x.mean + x.se, angle = 90, length = 0.1)
arrows(px, x.mean + x.se, px, x.mean - x.se, angle = 90, length = 0.1)

		dev.copy(pdf, file=paste(flname,"a.pdf",sep=""), width = 10, height = 10)
		dev.off()

if (enn == 5) {
	ha<-NULL
	ennb<-enna-1
	ha<-rbind(c(0,1,2),c(3,4,0),c(5,0,0))#
	for (i in 1:ennb){
		ja<-enna-i
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2　, y0 = xh- h -ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh- h -ts, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("***" ,signif(sd,digits=4))  , font = 1, cex = ft, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("**" ,signif(sd,digits=4)), font = 1,cex = ft, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("*" ,signif(sd,digits=4)) , font = 1,cex = ft, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+cs, labels =signif(sd,digits=4) , font = ft, cex = ft, adj = 0)
		} 	
	}
	}
}else if (enn == 4){
	ha<-NULL
	ennb<-enna-1
	ha<-rbind(c(0,1),c(2,0))#
	for (i in 1:ennb){
		ja<-enna-i
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh - h　-ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh - h -ts, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("***" ,signif(sd,digits=4)) , cex = ft , font = 1, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("**" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("*" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+cs, labels =signif(sd,digits=4) , cex = ft , font = 1, adj = 0)
		} 	
	}
	}
}else if (enn == 7){
	ha<-NULL
	ennb<-enna-1		#項目間の数
	ha<-rbind(c(0,1,2,3,4),c(5,6,7,8,0),c(9,10,11,0,0),c(12,13,0,0,0),c(14,0,0,0,0))#
	for (i in 1:ennb){
		ja<-enna-i	#ja:
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh - h　-ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh - h -ts, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("***" ,signif(sd,digits=4)) , cex = ft , font = 1, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("**" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("*" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+cs, labels =signif(sd,digits=4) , cex = ft , font = 1, adj = 0)
		} 	
	}
	}
}else if (enn == 6){
	ha<-NULL
	ennb<-enna-1		#項目間の数
	ha<-rbind(c(0,1,2,3),c(4,5,6,0),c(7,8,0,0),c(9,0,0,0))#
	for (i in 1:ennb){
		ja<-enna-i	#ja:
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh - h　-ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh - h -ts, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("***" ,signif(sd,digits=4)) , cex = ft , font = 1, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("**" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("*" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+cs, labels =signif(sd,digits=4) , cex = ft , font = 1, adj = 0)
		} 	
	}
	}
}else if (enn == 9){
	ha<-NULL
	ennb<-enna-1		#項目間の数
	ha<-rbind(c(0,1,2,3,4,5,6),c(7,8,9,10,11,12,0),c(13,14,15,16,17,0,0),c(18,19,20,21,0,0,0),c(22,23,24,0,0,0,0),c(25,26,0,0,0,0,0),c(27,0,0,0,0,0,0))#
	for (i in 1:ennb){
		ja<-enna-i	#ja:
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh - h　-ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
		segments(x0 = px[i,]-0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 = xh- h, lwd=0.5)
		segments(x0 = px[i+j,]+0.2 , y0 = xh- h, x1 = px[i+j,]+0.2, y1 =xh - h -ts, lwd=0.5)
		print(ha[i,j])
		hb<- ha[i,j]+1
		sd <- SD[hb]
		print(sd)
		if (sd < 0.001 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("***" ,signif(sd,digits=4)) , cex = ft , font = 1, adj = 0)
		}else if(sd < 0.01 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("**" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
		}else if(sd < 0.05 ){
			text(x = px[i,] , y = xh-h+cs, labels =paste("*" ,signif(sd,digits=4)), cex = ft , font = 1, adj = 0)
		}else{
			text(x = px[i,] , y = xh-h+cs, labels =signif(sd,digits=4) , cex = ft , font = 1, adj = 0)
		} 	
	}
	}
}	
	
	
		dev.copy(pdf, file=paste(flname,".pdf",sep=""), width = 10, height = 10)
		dev.off()

#}
#}

