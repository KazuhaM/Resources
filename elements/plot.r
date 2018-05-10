PL<-function(a1,a2=NULL,a3= NULL,a4=NULL){
if(is.null(a2)==TRUE && is.null(a3)==TRUE && is.null(a4)==TRUE){
	if(max(a1$count_5min)>=1){
		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)
		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))
		sink()  
		a1.p<-predict(a1.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		legend(1,max(a1$count_5min),
			as.character(a1$try_dune_height[1]),pch=1,col=1)

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()
	}else{
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		legend(1,max(a1$count_5min),
			as.character(a1$try_dune_height[1]),pch=1,col=1)
				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()
	}

}else if(is.null(a4)==TRUE || max(a4$count_5min)<1){
		x<-c(max(a1$count_5min)>=1,max(a2$count_5min)>=1,max(a3$count_5min)>=1)	
	if(x[1]==TRUE && x[2]==TRUE && x[3]==TRUE){
		
		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)
		a2.h<-hurdle(round(a2$count_5min) ~ a2$Wind_Speed_5min)
		a3.h<-hurdle(round(a3$count_5min) ~ a3$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))
			print(summary(a2.h))
			print(summary(a3.h))
		sink()  
		a1.p<-predict(a1.h)
		a2.p<-predict(a2.h)
		a3.p<-predict(a3.h)


		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		if(is.null(a4)==FALSE){
			par(new=T)
			plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
				xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
				ylim=c(0,max(a1$count_5min)*1.1),
				ann = F,axes = F, col=4)
			legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
			pch=c(1,1,1,1),col=c(1,2,3,4))
		}else{
		legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1])),
			pch=c(1,1,1),col=c(1,2,3))
		}
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		par(new=T)
		plot(a2$Wind_Speed_5min, a2.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min, a3.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=3)


				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==TRUE && x[2]==TRUE && x[3]==FALSE){
		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)
		a2.h<-hurdle(round(a2$count_5min) ~ a2$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))
			print(summary(a2.h))

		sink()  
		a1.p<-predict(a1.h)
		a2.p<-predict(a2.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		if(is.null(a4)==FALSE){
			par(new=T)
			plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
				xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
				ylim=c(0,max(a1$count_5min)*1.1),
				ann = F,axes = F, col=4)
			legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
			pch=c(1,1,1,1),col=c(1,2,3,4))
		}else{
		legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1])),
			pch=c(1,1,1),col=c(1,2,3))
		}
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		par(new=T)
		plot(a2$Wind_Speed_5min, a2.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=2)

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==TRUE && x[2]==FALSE && x[3]==TRUE){

		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)
		a3.h<-hurdle(round(a3$count_5min) ~ a3$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))
			print(summary(a3.h))
		sink()  
		a1.p<-predict(a1.h)
		a3.p<-predict(a3.h)


		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)

		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		if(is.null(a4)==FALSE){
			par(new=T)
			plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
				xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
				ylim=c(0,max(a1$count_5min)*1.1),
				ann = F,axes = F, col=4)
			legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
			pch=c(1,1,1,1),col=c(1,2,3,4))
		}else{
		legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1])),
			pch=c(1,1,1),col=c(1,2,3))
		}
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		par(new=T)
		plot(a3$Wind_Speed_5min, a3.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=3)
				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==TRUE && x[2]==FALSE && x[3]==FALSE){
		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))

		sink()  
		a1.p<-predict(a1.h)



		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		if(is.null(a4)==FALSE){
			par(new=T)
			plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
				xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
				ylim=c(0,max(a1$count_5min)*1.1),
				ann = F,axes = F, col=4)
			legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
			pch=c(1,1,1,1),col=c(1,2,3,4))
		}else{
		legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1])),
			pch=c(1,1,1),col=c(1,2,3))
		}
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==FALSE && x[2]==TRUE && x[3]==TRUE){
		
		a2.h<-hurdle(round(a2$count_5min) ~ a2$Wind_Speed_5min)
		a3.h<-hurdle(round(a3$count_5min) ~ a3$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a2.h))
			print(summary(a3.h))
		sink()  

		a2.p<-predict(a2.h)
		a3.p<-predict(a3.h)


		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		if(is.null(a4)==FALSE){
			par(new=T)
			plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
				xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
				ylim=c(0,max(a1$count_5min)*1.1),
				ann = F,axes = F, col=4)
			legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
			pch=c(1,1,1,1),col=c(1,2,3,4))
		}else{
		legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1])),
			pch=c(1,1,1),col=c(1,2,3))
		}
		par(new=T)
		plot(a2$Wind_Speed_5min, a2.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min, a3.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=3)

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()
		
	}else if(x[1]==FALSE && x[2]==TRUE && x[3]==FALSE){
		a2.h<-hurdle(round(a2$count_5min) ~ a2$Wind_Speed_5min)


		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  

			print(summary(a2.h))

		sink()  

		a2.p<-predict(a2.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		if(is.null(a4)==FALSE){
			par(new=T)
			plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
				xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
				ylim=c(0,max(a1$count_5min)*1.1),
				ann = F,axes = F, col=4)
			legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
			pch=c(1,1,1,1),col=c(1,2,3,4))
		}else{
		legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1])),
			pch=c(1,1,1),col=c(1,2,3))
		}
		par(new=T)
		plot(a2$Wind_Speed_5min, a2.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=2)

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()


	}else if(x[1]==FALSE && x[2]==FALSE && x[3]==TRUE){
		a3.h<-hurdle(round(a3$count_5min) ~ a3$Wind_Speed_5min)
	
		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a3.h))
		sink()  
		a3.p<-predict(a3.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		if(is.null(a4)==FALSE){
			par(new=T)
			plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
				xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
				ylim=c(0,max(a1$count_5min)*1.1),
				ann = F,axes = F, col=4)
			legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
			pch=c(1,1,1,1),col=c(1,2,3,4))
		}else{
		legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1])),
			pch=c(1,1,1),col=c(1,2,3))
		}

		par(new=T)
		plot(a3$Wind_Speed_5min, a3.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=3)
		par(new=T)


				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==FALSE && x[2]==FALSE && x[3]==FALSE){
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		if(is.null(a4)==FALSE){
			par(new=T)
			plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
				xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
				ylim=c(0,max(a1$count_5min)*1.1),
				ann = F,axes = F, col=4)
			legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
			pch=c(1,1,1,1),col=c(1,2,3,4))
		}else{
		legend(1,max(a1$count_5min),
			legend=c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1])),
			pch=c(1,1,1),col=c(1,2,3))
		}
				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()
	}

}else{
		x<-c(max(a1$count_5min)>=1,max(a2$count_5min)>=1,max(a3$count_5min)>=1,max(a4$count_5min)>=1)	
	if(x[1]==TRUE && x[2]==TRUE && x[3]==TRUE && x[4]==TRUE){
		
		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)
		a2.h<-hurdle(round(a2$count_5min) ~ a2$Wind_Speed_5min)
		a3.h<-hurdle(round(a3$count_5min) ~ a3$Wind_Speed_5min)
		a4.h<-hurdle(round(a4$count_5min) ~ a4$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))
			print(summary(a2.h))
			print(summary(a3.h))
			print(summary(a4.h))
		sink()  
		a1.p<-predict(a1.h)
		a2.p<-predict(a2.h)
		a3.p<-predict(a3.h)
		a4.p<-predict(a4.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=4)
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		par(new=T)
		plot(a2$Wind_Speed_5min, a2.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min, a3.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min, a4.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=4)
		legend(1,max(a1$count_5min),
			c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
				pch=c(1,1,1,1),col=c(1,2,3,4))

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

		
	}else if(x[1]==TRUE && x[2]==TRUE && x[3]==FALSE && x[4]==TRUE){
		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)
		a2.h<-hurdle(round(a2$count_5min) ~ a2$Wind_Speed_5min)
		a4.h<-hurdle(round(a4$count_5min) ~ a4$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))
			print(summary(a2.h))
			print(summary(a4.h))
		sink()  
		a1.p<-predict(a1.h)
		a2.p<-predict(a2.h)
		a4.p<-predict(a4.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=4)
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		par(new=T)
		plot(a2$Wind_Speed_5min, a2.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=2)
		par(new=T)
		plot(a4$Wind_Speed_5min, a4.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=4)
		legend(1,max(a1$count_5min),
			c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
				pch=c(1,1,1,1),col=c(1,2,3,4))

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==TRUE && x[2]==FALSE && x[3]==TRUE && x[4]==TRUE){
		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)
		a3.h<-hurdle(round(a3$count_5min) ~ a3$Wind_Speed_5min)
		a4.h<-hurdle(round(a4$count_5min) ~ a4$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))
			print(summary(a3.h))
			print(summary(a4.h))
		sink()  
		a1.p<-predict(a1.h)
		a3.p<-predict(a3.h)
		a4.p<-predict(a4.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=4)
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		par(new=T)
		plot(a3$Wind_Speed_5min, a3.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min, a4.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=4)
		legend(1,max(a1$count_5min),
			c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
				pch=c(1,1,1,1),col=c(1,2,3,4))

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==TRUE && x[2]==FALSE && x[3]==FALSE && x[4]==TRUE){
		a1.h<-hurdle(round(a1$count_5min) ~ a1$Wind_Speed_5min)
		a4.h<-hurdle(round(a4$count_5min) ~ a4$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a1.h))
			print(summary(a4.h))
		sink()  
		a1.p<-predict(a1.h)
		a4.p<-predict(a4.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=4)
		par(new=T)
		plot(a1$Wind_Speed_5min, a1.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=1)
		par(new=T)
		plot(a4$Wind_Speed_5min, a4.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=4)
		legend(1,max(a1$count_5min),
			c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
				pch=c(1,1,1,1),col=c(1,2,3,4))

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==FALSE && x[2]==TRUE && x[3]==TRUE && x[4]==TRUE){

		a2.h<-hurdle(round(a2$count_5min) ~ a2$Wind_Speed_5min)
		a3.h<-hurdle(round(a3$count_5min) ~ a3$Wind_Speed_5min)
		a4.h<-hurdle(round(a4$count_5min) ~ a4$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a2.h))
			print(summary(a3.h))
			print(summary(a4.h))
		sink()  
		a2.p<-predict(a2.h)
		a3.p<-predict(a3.h)
		a4.p<-predict(a4.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=4)
		par(new=T)
		plot(a2$Wind_Speed_5min, a2.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min, a3.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min, a4.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=4)
		legend(1,max(a1$count_5min),
			c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
				pch=c(1,1,1,1),col=c(1,2,3,4))

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()
		
	}else if(x[1]==FALSE && x[2]==TRUE && x[3]==FALSE && x[4]==TRUE){
		a2.h<-hurdle(round(a2$count_5min) ~ a2$Wind_Speed_5min)
		a4.h<-hurdle(round(a4$count_5min) ~ a4$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a2.h))
			print(summary(a4.h))
		sink()  
		a2.p<-predict(a2.h)
		a4.p<-predict(a4.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=4)
		par(new=T)
		plot(a2$Wind_Speed_5min, a2.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=2)
		par(new=T)
		plot(a4$Wind_Speed_5min, a4.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=4)
		legend(1,max(a1$count_5min),
			c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
				pch=c(1,1,1,1),col=c(1,2,3,4))

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else if(x[1]==FALSE && x[2]==FALSE && x[3]==TRUE && x[4]==TRUE){
		a3.h<-hurdle(round(a3$count_5min) ~ a3$Wind_Speed_5min)
		a4.h<-hurdle(round(a4$count_5min) ~ a4$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a3.h))
			print(summary(a4.h))
		sink()  
		a3.p<-predict(a3.h)
		a4.p<-predict(a4.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=4)
		par(new=T)
		plot(a3$Wind_Speed_5min, a3.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min, a4.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=4)
		legend(1,max(a1$count_5min),
			c(as.character(a1$try_dune_height[1]),as.character(a2$try_dune_height[1]),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
				pch=c(1,1,1,1),col=c(1,2,3,4))

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()

	}else{
		if(x[1]==FALSE && x[2]==FALSE && x[3]==FALSE && x[4]==TRUE){
		a4.h<-hurdle(round(a4$count_5min) ~ a4$Wind_Speed_5min)

		sink(paste("7_",a, "_",mn7,".txt",sep=""),append = TRUE)  
			print(summary(a4.h))
		sink()  
		a4.p<-predict(a4.h)

		#プロット
		windows( width = 9, height = 9)
		par(mar = c(5.2, 5, 6, 2.1),pty = "s",fin = c(8,8))
		plot(a1$Wind_Speed_5min,a1$count_5min,
			main=paste("Relationships between 
				Sand Drifting and Wind Speed
				",a, "_", mn7,sep=""), 
			xlab="Wind Speed (m/s)",
			ylab="Drifting Sands (n/s)",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			cex.lab  = 1.5,       #  軸の説明の字の大きさを設定する
  		   	cex.axis = 1.5,      #  軸の数字等（ラベル）の大きさを設定する
  		   	cex.main = 1.8,     #  メインタイトルの字の大きさを設定する
			col=1
		)
		par(new=T)
		plot(a2$Wind_Speed_5min,a2$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=2)
		par(new=T)
		plot(a3$Wind_Speed_5min,a3$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=3)
		par(new=T)
		plot(a4$Wind_Speed_5min,a4$count_5min , xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F, col=4)
		par(new=T)
		plot(a4$Wind_Speed_5min, a4.p, type="l", xlab="",ylab="",main="",
			xlim=c(0,max(a1$Wind_Speed_5min)*1.2),
			ylim=c(0,max(a1$count_5min)*1.1),
			ann = F,axes = F,col=4)
		legend(1,max(a1$count_5min),
			c(as.character(a1$try_dune_height[1]),as.character(as.character(a2$try_dune_height[1])),as.character(a3$try_dune_height[1]),as.character(a4$try_dune_height[1])),
				pch=c(1,1,1,1),col=c(1,2,3,4))

				dev.copy(pdf, file=paste("7_",a, "_",mn7,".pdf",sep=""), width = 10, height = 10)
				dev.off()
		}
}
}
}