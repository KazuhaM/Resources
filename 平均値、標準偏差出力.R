n<-1
x.sum<-list()
x.ln<-c()

for(u in 1 : 2){
for(v in 1 : 3){
for(w in 1 : 2){
#u<-1
#v<-1
#w<-1

a<-u
b<-v
c<-w

#ƒf[ƒ^“Ç‚Ýž‚Ý
A<-c("R","C")
B<-c("D","Y","L")
if (a==1){
	C<-c("b","d")
}else{
	C<-c("b","a")

}
if(a==1){
	lb<-"Ží"
	sn<-2
}else{
	lb<-"“"
	sn<-3
}

switch(a,               
   "1" = xl<-"Richness",
   "2" = xl<-"Cover"   
)
switch(b,               
   "1" = yl<-"Direction",
   "2" = yl<-"Year",
   "3" = yl<-"Layer"   
)
switch(c,               
   "1" = zl<-"Antiseeding",
   "2" = zl<-"Seeding"   
)

flname<-paste(A[a],C[c],"-",B[b], sep ="")
x.ln[n]<-paste(xl,yl,zl)

x<-read.csv(paste(flname,"5.csv",sep=""),header=TRUE)

enn<-ncol(x)
 
x.mean<-apply(x[1:enn-1], 2 , mean , na.rm=T)
x.se<-c()
hm<-c()
enna<-enn-1
for (i in 1 : enna){
	#if(is.na(names(table(is.na(x[,i])==FALSE)["TRUE"]))){
	#	hm[i]<-0
	#	break
	#	}else
	#	print(i)
		if(table(is.na(x[,i])==FALSE)["TRUE"]==length(x[,i])){
			hm[i]<-length(x[,i])
		}else {
			hm[i]<-table(is.na(x[,i])==FALSE)["TRUE"]
		}
	sda[i]<-sd(x[,i], na.rm=T)
	hma[i]<-sqrt(hm[i])
	x.se[i]<-sda [i]/ hma[i]
}

#x.sd <- apply(x[1:enn-1], 2, sd, na.rm = T)

x.suma<-c()
x.mean<-signif(x.mean, digits = sn)
x.se<-signif(x.se, digits = sn)
for(i in 1 : enna ){
	x.suma[i]<-paste(x.mean[i],"}",x.se[i],lb)
}
names(x.suma)<-names(x.mean)
x.sum[[n]]<-x.suma
n<-n+1
}}}

names(x.sum)<-x.ln
x.sum



