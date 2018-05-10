z<- y[!is.na(y)]
z
m<-length(y[1,])
m
length(y[1,])

for (i in 1:length(y[1,])){
	hm[i]<-length(y[!is.na(y[,i])])
}
hma<-rep(1:m,hm[])
hma
hm
a<-z[2:37]
b<-hma[2:37]
a
b<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3)
b

Steel.Dwass(a,b )

		i=1
		if (table(is.na(y[,i]==FALSE))==length(y[,i])){
			hm[i]<-length(y[,i])
		}else {
			hm[i]<-length(y[!is.na(y[,i])])
		}
#hm<-NULL
hm
hm[i]
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

	a<-Steel.Dwass(za[[13]],hmc[[13]])
	b<-Steel.Dwass(za[[13]],hmc[[13]])
	Steel.Dwass(za[[5]],hmc[[5]])
	za[[5]]
	hmc[[5]]
	a
	b


source("http://aoki2.si.gunma-u.ac.jp/R/src/Steel-Dwass.R", encoding="euc-jp")
data <- c(
	6.9, 7.5, 8.5, 8.4, 8.1, 8.7, 8.9, 8.2, 7.8, 7.3, 6.8, # 第 1 群のデータ，11 例
	9.6, 9.4, 9.5, 8.5, 9.4, 9.9, 8.7, 8.1, 7.8, 8.8,      # 第 2 群のデータ，10 例
	5.7, 6.4, 6.8, 7.8, 7.6, 7.0, 7.7, 7.5, 6.8, 5.9,      # 第 3 群のデータ，10 例
	7.6, 8.7, 8.5, 8.5, 9.0, 9.2, 9.3, 8.0, 7.2, 7.9, 7.8  # 第 4 群のデータ，11 例
	)
group <- rep(1:4, c(11, 10, 10, 11))  
group                   # 群の識別変数
Steel.Dwass(data, group)

# スティール・ドゥワス(Steel-Dwass)の方法による多重比較
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
        result <- cbind(t, p)                                                # 結果をまとめる
        rownames(result) <- combn(ng, 2, paste, collapse=":")
        return(result)
}
Steel.Dwass


	
	hmc

	hmc[[1]]<-rep(1:3,hm[1:3])
	hmc[[1]]

	hmb
	hm
	hma
	z[2:37]
	z[38:45]


	za<-list()
	za[[1]]<-z[1:1]
	za[[2]]<-z[2:37]
	za[[3]]<-z[38:45]
	za
	z[]
	
	za<-list()
	for(i in 1:length(hmb)){
		if (i == 1){
			za[[1]]<-z[1:hmb[1]]
		}else{
			k=hmb[i-1]+1
			l=hmb[i]
			za[[i]]<-z[k:l]
		}
	}	
	za
	z[2:37]
	za[[2]]
	length(za[[3]])
	z[38:45]
	za[[3]]
	
		
	za<-matrix(,length(hm),max(hm))
	za[1,1]<-z[1]
	for (i in 1 : 36){
		za[i,2]<-z[1+i]
	}
	for(i in 1:)
	za[i]<-list(z[,i])
	
	za[,2]
	za
	zb<-na.omit(za)
	zb	
	za[,1]
	za[,2]
	zb<-complete.cases(za[,2])
	zb<-t(zb)
	zb
	za[,2]<-za[zb,2]

	za[,1]<-z[1]
	za[,1]
	za[,1]<-c(0,1,2,3,4,5,6,7,8,9)
	za[,2]<-z[2:37]
	za<-array(0,dim=c(length(hm),max(hm)))
	za<-NULL
	za
	ya<-y[!is.na(y[,6])]
	ya
	ya[1]<-paste(ya[1],y[!is.na(y[,2])])
	ya[1]<-ya[1]+y[!is.na(y[,3])]
	ya[1]
	ya[1]<-c(y[!is.na(y[,1])],y[!is.na(y[,2])],y[!is.na(y[,3])])
y[!is.na(y[,1])]

