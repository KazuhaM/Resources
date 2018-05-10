wilcoxonの検定読み込み
	install.packages("exactRankTests", repos="http://cran.ism.ac.jp/")
	library(exactRankTests)
EN<-c("D","L","Y")
EL<-c("c","f","h","n")
#ENl<-length(EN)
#ELl<-length(EL)
#flname<-paste(EN[1],"-",EL[2],".csv", sep = "")
#flname

#for(i in 1 : 3){
#for(j in 1 : 4){

A<-3
B<-3

# データを読み込む
	flname<-paste(EN[A],"-",EL[B],".csv", sep = "")
	x <- read.csv(flname, header = TRUE)
	y <- x[, -1]                         # データを取り出して y に保存する
	type <- unique(x[, 1])               # 種を取得
	n <- length(type)-1			　#種数の取得
	yn<-ncol(y)					#yの項目数取得					



#ラベル作成
	name<-colnames(x)
	en<-substring(name[1],1,1)
	el<-substring(name[1],3,3)
	switch(en,               
	   "D" = xl<-"Direction",
	   "Y" = xl<-"Year",
	   "L" = xl<-"Layer"   
	)
	switch(el,               
	   "c" = yl<-"cover",
	   "h" = yl<-"height",
	   "n" = yl<-"number",
	   "f" = yl<-"flower"   
	)

#各種に欠損地が含まれるかを判定
naor<-c()
for (i in 1:yn){
	naor[i]<-is.na(y[1,i])
}

#各種が二項検定なのか、多重比較なのかの判定
	testw<-c()
	for(i in 1:n){
		k=3*i-2
		l=3*i-1
		m=3*i
		d<-table(naor[k:m]==FALSE)
		if (d[["TRUE"]]==2){
			testw[i]<-2		
		}else if (d[["TRUE"]]==3){
			testw[i]<-3
		}else{
			testw[i]<-1
		}
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

#Stell.Dwass検定用配列作成
	z<- y[!is.na(y)]	#欠損値なしの一次配列作成

	for (i in 1:yn){		#各項目の要素数を取得
		#print(i)
		if(is.na(names(table(is.na(y[,i])==FALSE)["TRUE"]))){
		hm[i]<-0
		}else if(table(is.na(y[,i])==FALSE)["TRUE"]==length(y[,i])){
			hm[i]<-length(y[,i])
		}else {
			#hm[i]<-length(y[!is.na(y[,i])])
			hm[i]<-table(is.na(y[,i])==FALSE)["TRUE"]
		}
	}
	
	hmb <- c()			#項目数の三つ切り累計を計算
	for (i in 1 : n){
		o<-3*i
		hmb[i]<- sum(hm[1:o])
	} 

	za<-NULL
	za<-list()			#データを種ごとに分割した一時配列の作成
	for(i in 1:length(hmb)){
		if (i == 1){
			za[[1]]<-z[1:hmb[1]]
		}else{
			k=hmb[i-1]+1
			l=hmb[i]
			za[[i]]<-z[k:l]
		}
	}

	hmc<-NULL
	hmc<-list()			#各種のデータをグループ分け
	for (i in 1:n){
		k=3*i-2
		l=3*i
		hmc[[i]] <- rep(1:3,hm[k:l])
	}

#wilcoxon検定用配列作成
	hmaa <- c()			#項目数の単純累計を計算
	for (i in 1 : yn){
		hmaa[i]<- sum(hm[1:i])
	}

	ya<-NULL
	ya<-list()			#欠損地なしの配列作成
	for (i in 1 : yn){
		if (hm[i]==0){
		}else if(i==1){
			ya[[i]]<-z[1:hmaa[i]]
		}else {
			k=hmaa[i-1]+1
			l=hmaa[i]
			ya[[i]]<-z[k:l]	

		}
	}


#種ごとに検定
	sd<-NULL
	sd<-list()
	for (i in 1 :n) {
		if (testw[i]==2){
			E=3*i-2
			N=3*i-1
			W=3*i
			if(naor[E]=="TRUE"){
				sda<-wilcox.exact(ya[[N]],ya[[W]],paired=F)
				sd[[i]]<-sda$p.value
			}else if(naor[N]=="TRUE"){
				sda<-wilcox.exact(ya[[W]],ya[[E]],paired=F)
				sd[[i]]<-sda$p.value
			}else if(naor[W]=="TRUE"){
				sda<-wilcox.exact(ya[[E]],ya[[N]],paired=F)
				sd[[i]]<-sda$p.value
			}			
		}else if (testw[i]==3){
			sd[[i]]<-Steel.Dwass(za[[i]],hmc[[i]])
		}
	}

# 各項目それぞれについてボックスプロットを描く
	#png( file= paste( en,"-", el, ".pdf"),width = 500, height =400)
	par(oma = c(0, 0, 4, 0)) #余白の設定
	if (n<=15){			 #ウィンドウの分割
		par(mfcol=c(3,5))
	}else if(n>15 && n<=20){
		par(mfcol=c(4,5))
	}else {
		print("ERROR")
	}
	for (i in 1:n) {
		k=3*i-2
		l=3*i
		mx<-max(y[,k:l],na.rm = TRUE)
		mx<-mx*1.1
		mn<-min(y[,k:l],na.rm = TRUE)
		mn<-mn*0.9
		plot(0, 0, type = "n",  xlim=range(0:4),
			ylim = c( mn , mx ) , xlab = xl, 
				ylab = yl, axes = FALSE)
		boxplot(y[,3*i-2],y[,3*i-1],y[,3*i],
      		col = "white" , xaxt = "n", add = TRUE, boxwex = 0.7, type = "n",
				main=type[i], xlab = xl, ylab = yl)
		axis(1, at = 1:3, labels = name[2:4], tick = TRUE)

		# 線を加える
		mxa<- mx*0.97
		mxb<- mx *0.87
		mxc<- mx *0.77
		if (length(sd[[i]])==0){
		}else if(length(sd[[i]])==3){
			segments(x0 = 0.9, y0 = mxa*0.93, x1 = 0.9, y1 = mxa,lwd=0.5)  #xとyの数値で始点と終点を指定
			segments(x0 = 0.9, y0 = mxa, x1 = 3.1, y1 = mxa,lwd=0.5)
			segments(x0 = 3.1, y0 = mxa, x1 = 3.1, y1 = mxa*0.93,lwd=0.5)
			text(x = 2, y = mxa, labels =signif(sd[[i]][1],digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0.5)
			if(sd[[i]][1] == "NaN"){
			}else if( sd[[i]][1]<=0.05){
				text(x = 1.1, y = mxa+1, labels ="*" , font = 1, ps = 10, adj = 0.5)
			}
	
			segments(x0 = 0.9, y0 = mxb*0.93, x1 = 0.9, y1 = mxb,lwd=0.5)  #xとyの数値で始点と終点を指定
			segments(x0 = 0.9, y0 = mxb, x1 = 2.1, y1 = mxb,lwd=0.5)
			segments(x0 = 2.1, y0 = mxb, x1 = 2.1, y1 = mxb*0.93,lwd=0.5)
			text(x =1.4, y = mxb, labels =signif(sd[[i]][2],digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0.5)
			if(sd[[i]][2]=="NaN" ){}else if( sd[[i]][2]<=0.05){
				text(x = 1.1, y = mxb+1, labels ="*" ,  font = 1, ps =10, adj = 0.5)
			}
	
			segments(x0 = 1.9, y0 = mxc*0.93, x1 = 1.9, y1 = mxc,lwd=0.5)  #xとyの数値で始点と終点を指定
			segments(x0 = 1.9, y0 = mxc, x1 = 3.1, y1 = mxc,lwd=0.5)
			segments(x0 = 3.1, y0 = mxc, x1 = 3.1, y1 = mxc*0.93,lwd=0.5)
			text(x = 2.6, y = mxc, labels =signif(sd[[i]][3],digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0.5)
			if(sd[[i]][3]=="NaN" ){}else if( sd[[i]][3]<=0.05){
				text(x = 2.1, y = mxc+1, labels ="*" , font = 1, ps = 10, adj = 0.5)
			}
		}else if (length(sd[[i]])==1){
			E=3*i-2
			N=3*i-1
			W=3*i
			if(naor[E]=="TRUE"){
				mx<-mx*0.98
				segments(x0 = 1.9, y0 = mx*0.95, x1 = 1.9, y1 = mx,lwd=0.5)  #xとyの数値で始点と終点を指定
				segments(x0 = 1.9, y0 = mx, x1 = 3.1, y1 = mx,lwd=0.5)
				segments(x0 = 3.1, y0 = mx, x1 = 3.1, y1 = mx*0.95,lwd=0.5)
				text(x = 2.6, y = mx, labels =signif(sd[[i]][1],digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0.5)
			if(sd[[i]][1]=="NaN" ){}else if( sd[[i]][1]<=0.05){
				text(x = 2.1, y = mxa+1, labels ="*"  , font = 1, ps = 10, adj = 0.5)
			}
			}else if(naor[N]=="TRUE"){
				mx<-mx*0.98
				segments(x0 = 0.9, y0 = mx*0.95, x1 = 0.9, y1 = mx,lwd=0.5)  #xとyの数値で始点と終点を指定
				segments(x0 = 0.9, y0 = mx, x1 = 3.1, y1 = mx,lwd=0.5)
				segments(x0 = 3.1, y0 = mx, x1 = 3.1, y1 = mx*0.95,lwd=0.5)
				text(x = 2, y = mx, labels =signif(sd[[i]][1],digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0.5)
			if(sd[[i]][1]=="NaN" ){}else if( sd[[i]][1]<=0.05){
				text(x = 1.1, y = mxa+1, labels ="*" , font = 1, ps = 10, adj = 0.5)
			}
			}else if(naor[W]=="TRUE"){
				mx<-mx*0.98
				segments(x0 = 0.9, y0 = mx*0.95, x1 = 0.9, y1 = mx,lwd=0.5)  #xとyの数値で始点と終点を指定
				segments(x0 = 0.9, y0 = mx, x1 = 2.1, y1 = mx,lwd=0.5)
				segments(x0 = 2.1, y0 = mx, x1 = 2.1, y1 = mx*0.95,lwd=0.5)
				text(x =1.4, y = mx, labels =signif(sd[[i]][1],digits=4) , cex = 0.8 , font = 1, ps = 8, adj = 0.5)
			if(sd[[i]][1]=="NaN" ){}else if( sd[[i]][1]<=0.05){
				text(x = 1.1, y = mxa+1, labels ="*" ,  font = 1, ps = 10, adj = 0.5)
			}			
			}
		}

	}	
		dev.copy(pdf, file= paste( en , "-" , el , ".pdf" , sep = ""),width = 13, height = 10)
		dev.off()
		
#}
#}

