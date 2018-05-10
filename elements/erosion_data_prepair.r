data<-read.csv("16-17erosion2.csv",header=T)
ch<-NULL
ch<-c((1:length(data$check))[!is.na(data$check)==TRUE],length(data$check)+1)
lch<-length(ch)-1
for (i in 1 : lch){
	par(mar = c(5.1, 4.5, 4.1, 2.1))
	plot(data[ch[i]:ch[i+1]-1,3],data[ch[i]:ch[i+1]-1,2],
	main=paste(data$year[ch[i]],"_",data$try_dune_height[ch[i]],sep=""),
	xlab="Wind Speed(m/s)",ylab="Drifting Sands(n)",
	cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
     	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
     	cex.main = 1.8)      #  メインタイトルの字の大きさを設定する
	axis(side=1,tck=1.0,lty="dotted",cex.axis = 1.5)
	axis(side=2,tck=1.0,lty="dotted",lwd=2,cex.axis = 1.5)
	dev.copy(pdf, file=paste(data$year[ch[i]],"_",data$try_dune_height[ch[i]],".pdf"
	,sep=""), width = 10, height = 10)
	dev.off()
} 
th<-read.csv("threshold_WS.csv",header=T)
temp1<-data[!is.na(data$count),]
write.csv(temp1,"C:/OneDrive - g.ecc.u-tokyo.ac.jp/LEP/2017/現行資料/11第6回ゼミ発表/飛砂計-植被率/解析/test.csv")
j=0
data2<-NULL
library(tcltk)
pb <- txtProgressBar(min = 1, max = length(temp1$year), style = 3)
for(i in 1 : length(temp1$year)){
	setTxtProgressBar(pb, i) 
	if(!is.na(temp1$check[i])){
		j=j+1
		if(temp1$Wind.Speed[i] >= th$th_WS[j]){
			data2<-rbind(data2,temp1[i,])
		}
	}else{
		if(temp1$Wind.Speed[i] >= th$th_WS[j]){
			data2<-rbind(data2,temp1[i,])
}
}}






