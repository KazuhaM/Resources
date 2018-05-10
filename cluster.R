read.table("fcl3.txt",header=T)->fcl
fcl
fcl.dist <- dist(fcl,"manhattan")
fcl.dist
fcl.clust<-hclust(fcl.dist,"average")
plot(fcl.clust)

read.table("gcl2.txt",header=T)->glc
glc
glc.dist <- dist(glc,"manhattan")
glc.dist
glc.clust<-hclust(glc.dist,"average")
plot(glc.clust)

read.table("hcl.txt",header=T)->hcl
hcl
hcl.dist <- dist(hcl,"manhattan")
hcl.dist
hcl.clust<-hclust(hcl.dist,"average")
hcl.clust
plot(hcl.clust)


read.table("hcl.txt",header=T)->hcl
hcl
d.dist <- dist(d,"manhattan")
d.dist
d.clust<-hclust(d.dist,"average")
d.clust
plot(d.clust)