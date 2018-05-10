read.csv("回帰木CV.csv",header=T)->回帰木
回帰木.dist<- dist(回帰木,"manhattan")
回帰木.clust<- hclust(回帰木.dist,"average")
plot(回帰木.clust)

read.csv("回帰木2015cv.csv",header=T,row.names=1) -> d
read.csv("回帰木2015.csv",header=T) -> d
cv.dist <- dist(d,"manhattan") #各区画ごとのcvの違いを距離（ユークリッド距離）に直す
cv.clust <- hclust(cv.dist,"average")　#ユークリッド距離の近いものからまとめていく
plot(cv.clust)