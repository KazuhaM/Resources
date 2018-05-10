#for (i in 1 : 3){
#for (j in 1 : 4){ 
#a<-i
#b<-j

a<-1
b<-4

#�����p
ms<-1.3		#�O���t�̏���̔{���A
mxs<-1		#ms�ɉ�����l�A�O���t�̏�[
sas<-0.7		#�������Ԃ̋���
ts<-0.2		#�������̍��E�̒���
cs<-0.3			#p�l�̈ʒu
ft<-0.8

#�f�[�^�ǂݍ���
A<-c("er")
B<-c("d","l","m","y")

yl<-"ErosionPin (cm)"

switch(b,               
   "1" = xl<-"Direction",
   "2" = xl<-"Layer",
   "3" = xl<-"Mound",
   "4" = xl<-"Year"    
)
flname<-paste(A[a],"-",B[b], sep ="")

x<-read.csv(paste(flname,".csv",sep=""),header=TRUE)
enn<-ncol(x)


 
x.mean<-apply(x[1:enn-1], 2 , mean , na.rm=T)
x.se<-c()
hm<-c()
sda<-c()
hma<-c()
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

#Steel.Dwass�p�z��
y<-x[!is.na(x[,1:enn-1])]
hmb<-x[,enn]
y<-y[1:length(hmb)]


#Steel.Dwass�̎��̏o�͕ύX
	Steel.Dwass <- function(data,                                                # �f�[�^�x�N�g��
	                        group)                                          # �Q�ϐ��x�N�g��
	{
	        OK <- complete.cases(data, group)                            # �����l�����P�[�X������
	        data <- data[OK]
	        group <- group[OK]
	        n.i <- table(group)                                          # �e�Q�̃f�[�^��
	        ng <- length(n.i)                                            # �Q�̐�
	        t <- combn(ng, 2, function(ij) {
	                i <- ij[1]
	                j <- ij[2]
	                r <- rank(c(data[group == i], data[group == j]))     # �Q i, j ���܂Ƃ߂ă����N�t��
	                R <- sum(r[1:n.i[i]])                                        # ���蓝�v��
	                N <- n.i[i]+n.i[j]                                   # ��Q�̃f�[�^���̍��v
	                E <- n.i[i]*(N+1)/2                                  # ���蓝�v�ʂ̊��Ғl
	                V <- n.i[i]*n.i[j]/(N*(N-1))*(sum(r^2)-N*(N+1)^2/4)  # ���蓝�v�ʂ̕��U
	                return(abs(R-E)/sqrt(V))                                # t �l��Ԃ�
	        })
	        p <- ptukey(t*sqrt(2), ng, Inf, lower.tail=FALSE)            # P �l���v�Z
	        #result <- cbind(t, p)                                                # ���ʂ��܂Ƃ߂�
	        #rownames(result) <- combn(ng, 2, paste, collapse=":")
	        return(p)
		  #return(result)
	}
#Steel.Dwass(y,hmb)
SD <- Steel.Dwass(y,hmb)

#�_�O���t�̋L�q
xh<-max(x.mean + x.se)*ms
px <- barplot(x.mean, ylab = yl, xlab = xl, ylim = c(0,xh+mxs), ps = ft)
#�덷�o�[��`���Alength�͌덷�o�[�̃o�[�̒����������Aangle���s�p�ɂ���ƃo�[�͖���ƂȂ�
arrows(px, x.mean - x.se, px, x.mean + x.se, angle = 90, length = 0.1)
arrows(px, x.mean + x.se, px, x.mean - x.se, angle = 90, length = 0.1)

if (enn == 5) {
	ha<-NULL
	ennb<-enna-1
	ha<-rbind(c(0,1,2),c(3,4,0),c(5,0,0))#
	for (i in 1:ennb){
		ja<-enna-i
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2�@, y0 = xh- h -ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
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
		segments(x0 = px[i,]-0.2, y0 = xh - h�@-ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
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
	ennb<-enna-1		#���ڊԂ̐�
	ha<-rbind(c(0,1,2,3,4),c(5,6,7,8,0),c(9,10,11,0,0),c(12,13,0,0,0),c(14,0,0,0,0))#
	for (i in 1:ennb){
		ja<-enna-i	#ja:
	for (j in 1:ja){
		h<-sas *ha[i,j]
		segments(x0 = px[i,]-0.2, y0 = xh - h�@-ts, x1 = px[i,]-0.2, y1 = xh- h, lwd=0.5) 
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
	boxplot(x[,1:enna], ylab = yl, xlab = xl,ps = ft)
		dev.copy(pdf, file=paste(flname,"b.pdf",sep=""), width = 10, height = 10)
		dev.off()	