#各地点のデータ 
sample1 <- c(1, 1, 3, 1, 0, 1, 20, 66) 
sample2 <- c(0, 0, 2, 0, 9, 0, 1, 0) 
sample3 <- c(0, 0, 0, 5, 6, 0, 1, 0) 
sample4 <- c(3, 0, 0, 2, 0, 0, 0, 0) 

#１つにまとめる・行名もつける
bcm <- rbind(sample1=sample1, sample2=sample2, sample3=sample3, sample4=sample4) 

#列名をつける 
colnames(bcm) <- c("cardinals", "roadrunners", "bluebirds", "phoebes",   
				"titmice", "redtails", "chickadees", "waxwings") 
bcm

#vegan ライブラリーのインストールとロード 
install.packages("vegan") #初回だけでよい 
library(vegan)
install.packages("permute")

bcm.pca <- rda(bcm) #主成分分析の実行、結果を bcm.pca に保存 
bcm.pca   #結果を表示

prcomp(bcm)
plot(bcm.pca)

biplot(bcm.pca, scaling=1)


#まず非類似度行列を構成する 
bcm.dst <- vegdist(bcm)  # 非類似度の計算にデフォルトでは Bray-Curtis 指標が用いられる。

bcm.nmds <- monoMDS(bcm.dst)  # NMDS を行う。 
bcm.nmds 

plot(bcm.nmds, type="t") # 結果を図に表示する。 type="t"と指定し地点名でプロットする。
stressplot(bcm.nmds) # Shepard diagram を描く。 


bcm.nmds <- metaMDS(bcm.dst) 
plot(bcm.nmds, type="t") 
stressplot(bcm.nmds) 

# metaMDS 関数を群集データ行列に直接適用する（metaMDS 関数のなかで非類似度行列が計算される） 
bcm.nmds <- metaMDS(bcm) 
plot(bcm.nmds, type="t")



