## Modified TWINSPAN on traditional Ellenberg's Danube meadow dataset,
## projected on DCA and compared with original classification into
## three vegetation types made by tabular sorting:
library (twinspanR)
library (vegan)
data (danube)
danube
d<-read.table("twinspan3.txt",header=T)
p<-read.table("place2.txt",header=T)
d<-read.table("twinspan3.txt")
p<-read.table("place2.txt",header=F)
d<-read.csv("twinspan3.csv",row.names=1)
d<-read.csv("place2.csv",row.names=1)
res <- twinspan (d, modif = TRUE, clusters = 4)
k <- cut (res)
res
k
plot(k)
d
p 
$veg.type
dca <- decorana (d)
par (mfrow = c(1,2))
ordiplot (dca, type = 'n', display = 'si', main = 'Modified TWINSPAN')
points (dca, col = k)
for (i in c(1,2,4)) ordihull (dca, groups = k, show.group = i, col = i,
 draw = 'polygon', label = TRUE)
ordiplot (dca, type = 'n', display = 'si', main = 'Original assignment\n (Ellenberg 1954)')
points (dca, col = p$AllCover)
for (i in c(1:3)) ordihull (dca, groups = p$AllCover,
 show.group = unique (p$AllCover)[i], col = i,
 draw = 'polygon', label = TRUE)
biplot(dca)
res.clus<-hclust(res,"average")
res.clus

install.packages ('devtools')
devtools::install_github("zdealveindy/twinspanR")
help(data)
danube$env$veg.type
danube$env
danube$env$veg
danube$spe
danube$veg
danube$veg.type
danube
d$veg
p$veg.type
p$veg
p.type
p
