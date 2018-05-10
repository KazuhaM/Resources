	install.packages("exactRankTests", repos="http://cran.ism.ac.jp/")
	library(exactRankTests)

	x=c(0.5,0.5,0.5,0.5,1,1,1,1,2,4,4,4,4,5,5,5,5)
	y=c(0.5,0.5,4,6,9)
	z=c(1,1,1,1,2,3,3,3,3,4,4,4,5)
	x<-t(x)
x<-NULL
y<-NULL
z<-NULL
mean(x)
mean(y)
mean(z)
x
				sda<-wilcox.exact(ya[[14]],ya[[15]],paired=F)
				names(sda)
				sd[[5]]<-sda$p.value


#wilcoxon時の三つのうちどの二つを使うべきか判定する関数

	wiltes <- function(f) {
			E=3*f-2
			N=3*f-1
			W=3*f
			if(naor[E]=="TRUE"){
				sda<-wilcox.exact(ya[[N]],ya[[W]],paired=F)
				sd[[f]]<-sda$p.value
			}else if(naor[N]=="TRUE"){
				sda<-wilcox.exact(ya[[W]],ya[[E]],paired=F)
				sd[[f]]<-sda$p.value
			}else if(naor[W]=="TRUE"){
				sda<-wilcox.exact(ya[[E]],ya[[N]],paired=F)
				sd[[f]]<-sda$p.value
			}
	} 
for (i in 2:11){
	wiltes(i)
}
sd

 t.test(x,y,var.equal=T)
 t.test(y,z,var.equal=T)
 t.test(z,x,var.equal=T)
 wilcox.test(x,y,var.equal=T)
 wilcox.test(y,z,var.equal=T)
 wilcox.test(z,x,var.equal=T)

	wilcox.exact(x,y,paired=F)
	wilcox.exact(y,z,paired=F)
	wilcox.exact(z,x,paired=F)
length(x)
length(y)
length(z)
X<-c(0.5,0.5,0.5,0.5,1,1,1,1,2,4,4,4,4,5,5,5,5,0.5,0.5,4,6,9,1,1,1,1,2,3,3,3,3,4,4,4,5)
Y<-c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3)
Steel.Dwass(X,Y)
	help(wilcox.exact)	
	c<- wilcox.exact(x,y,paired=F)	
	c$p.value

	d<-wilcox.test(x,y)
	d$p.value
	
#各種に欠損地が含まれるかを判定
naor<-c()
for (i in 1:ncol(y)){
	naor[i]<-is.na(y[1,i])
}	
naor

d<-table(naor[1:3]==FALSE)
d
names(d)
d[["TRUE"]]
d[["FALSE"]]

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
length(testw)