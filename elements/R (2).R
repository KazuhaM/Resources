a<- read.csv("rawforR.csv")
ar<-c(a[36,2],a[36,3],a[36,4],a[36,5],a[36,6],a[36,7])
as<-mean(ar)
as
b<-NULL
b<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
h<-1
bs<-NULL
for(i in 2 :6){
	for(j in (i+1):7){
		for(k in 1:35){
			if(a[k,8]!=0 &(a[k,i]==1 || a[k,j]==1))
				b[h]<-b[h]+1
		}
	h<-h+1
	}
}
}
b
bs<-mean(b)
bs

c<-NULL
c<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
h<-1
cs<-NULL
for(i in 2 :5){
	for(j in (i+1):6){
		for(l in (j+1):7){
		for(k in 1:35){
			if(a[k,8]!=0 &(a[k,i]==1 || a[k,j]==1 || a[k,l]==1))
				c[h]<-c[h]+1
		}
		h<-h+1
		}
	}
}
c
cs<-mean(c)
cs

d<-NULL
d<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
h<-1
ds<-NULL
for(i in 2 :4){
	for(j in (i+1):5){
		for(l in (j+1):6){
		for(m in (l+1):7){
		for(k in 1:35){
			if(a[k,8]!=0 &(a[k,i]==1 || a[k,j]==1 || a[k,l]==1 ||a[k,m]==1))
				d[h]<-d[h]+1
		}
		h<-h+1
		}
		}
	}
}
d
ds<-mean(d)
ds

e<-NULL
e<-c(0,0,0,0,0,0)
h<-1
es<-NULL
for(i in 2 :3){
	for(j in (i+1):4){
		for(l in (j+1):5){
		for(m in (l+1):6){
		for(n in (m+1):7)
		for(k in 1:35){
			if(a[k,8]!=0 &(a[k,i]==1 || a[k,j]==1 || a[k,l]==1 ||a[k,m]==1 || a[k,n]==1))
				e[h]<-e[h]+1
		}
		h<-h+1
		}
		}
		}
	}
}
e
es<-mean(e)
es
fs<-35
sum<-c(as,bs,cs,ds,es,fs)
sum
plot(sum)
plot(sum, type="b", xlab="plot", ylab="species",col="blue")










