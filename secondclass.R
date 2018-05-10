read.table("crops.txt",header=T)->crop
crop
plot(crop)
crop.dist <- dist(crop,"manhattan")
crop.dist
crop.clust <- hclust(crop.dist,"average")
plot(crop.clust)
plot(crop.clust,main="",sub="",xlab="",ylab="¬•ª—Ê‚Ì·")