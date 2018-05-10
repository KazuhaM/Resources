
#データ読み込み
data<-read.csv("dataR.csv",header=T)
data[,3]<-as.character(data[,3])
data[,2]<-as.character(data[,2])
gd <- array(dim=c(20,5,11,2))
a<-array(1,dim =c(11,5,2))

grouping <- function(x,y,z,g){
	gd[a[y,x,g],x,y,g] <- data[z,y+3]
	a[y,x,g] <-a[y,x,g]+1
	return(list(gd,a))
	if(is.na(gd[a[y,x,g],x,y,g])==TRUE){return("ERROR")
	}
}
for(k in 1 : 2){
for(i in 1 : 11){			#iは環境条件の種類（11種）
	for(j in 1 : 20){		#jは縦列、20プロット
		if(data[j,k+1]==1){
			result <- grouping(1,i,j,k)
			gd <- result[[1]]
			a <- result[[2]]
		}else if(data[j,k+1]==2){
			result <- grouping(2,i,j,k)
			gd <- result[[1]]
			a <- result[[2]]
		}else if(data[j,k+1]==3){
			result <- grouping(3,i,j,k)
			gd <- result[[1]]
			a <- result[[2]]
		}else if(data[j,k+1]==4){
			result <- grouping(4,i,j,k)
			gd <- result[[1]]
			a <- result[[2]]
		}else if(data[j,k+1]==5){
			result <- grouping(5,i,j,k)
			gd <- result[[1]]
			a <- result[[2]]
		}else{
			print("ERROR")
		}
	}
}}


#各要因グラフ化
xl <- c("DCA Group", "Selected Group")
yl <- c("Lux","SoilHardness(Pa)","SoilWater(Vol%)","SoilEC(μS/cm)","SoilTemp(℃)",
	"Richness(n)","SumCover(%)","SimpsonD","SimponN","ShannonH","ShannonJ")
xla<-c("DG","SG")
yla<-c("L","SH","SW","SEC","ST","R","SC","SimD","SimN","ShanH","Shanj")
for(i in 1 : 2){
	mfrow = c(3,4)
for(j in 1 : 11){
	boxplot(gd[,,j,i],xlab = xl[i], ylab = yl[j])
	dev.copy(pdf, file=paste(xla[i],yla[j],".pdf",sep=""))
	dev.off()
}
	mfrow=c(1,1)
}

#各要因グラフ化(まとめてpdf).
xlb<-c("DCAGsum","SelGsum")
for(i in 1 : 2){
	par(mfrow = c(3,4))
for(j in 1 : 11){
	boxplot(gd[,,j,i],xlab = xl[i], ylab = yl[j])
}
	dev.copy(pdf, file=paste(xlb[i],".pdf",sep=""))
	dev.off()
	par(mfrow=c(1,1))
}
dev.off()



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

#Steel.Dwassを各要因に対して
d<-NULL
gr<-NULL
SD<-array(dim=c(10,11,2))
for(i in 1 : 2){
	for (j in 1 : 11){
		for(k in 1 : 5){
			d <- c(d,gd[!is.na(gd[,k,j,i]),k,j,i])
			gr <- c(gr,rep(k,length(gd[!is.na(gd[,k,j,i]),k,j,i])))
		}
		if(gr[20]==4){
			SD[1:6,j,i] <- Steel.Dwass(d,gr)
			d<-NULL
			gr<-NULL
		}else if(gr[20]==5){
			SD[1:10,j,i] <- Steel.Dwass(d,gr)
			d<-NULL
			gr<-NULL
		}else{
			return("ERROR")
			d<-NULL
			gr<-NULL
		}
	}
}
SD

#Tukey-Kramerを各要因に対して
d<-NULL
gr<-NULL
TK<-as.list(NULL)
for(i in 1 : 2){
	for (j in 1 : 11){
		for(k in 1 : 5){
			d <- c(d,gd[!is.na(gd[,k,j,i]),k,j,i])
			gr <- factor(c(gr,rep(k,length(gd[!is.na(gd[,k,j,i]),k,j,i]))))
		}
		if(gr[20]==4){
			TK <- append(TK,list(TukeyHSD(aov(d~gr))))
			names(TK[[j]]) <-paste(xl[i],yl[j])
			d<-NULL
			gr<-NULL
		}else if(gr[20]==5){
			TK <- append(TK,list(TukeyHSD(aov(d~gr))))
			names(TK[[11+j]]) <-paste(xl[i],yl[j])
			d<-NULL
			gr<-NULL
		}else{
			return("ERROR")
			d<-NULL
			gr<-NULL
		}
	}
}
TK
write.table(TK, "TK.txt", append = T)

pairs(data[,4:14])

plot(data$ShannonJ~data$SoilHardness)
result <- lm(data$ShannonJ~data$SoilHardness)
abline(result)
summary(result)



