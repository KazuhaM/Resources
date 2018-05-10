
# 読み込み
species.data <- read.table("species.csv", header=T) new <- rep(species.data$species,species.data$paddy)  species <- t(species.data)
e <- rep(species$species,species$e)  
# ヒストグラム resamp <- sapply(1:1000, function(x) length( levels( factor( sample(new, #江の個体数) ) ) ))
 epar <- par(no.readonly = TRUE) par(mar=c(5,5,0,0)) hist(resamp, main="", xlab="種数", ylab="シミュレーション数", col="gray")
par(oldpar) 

# 希薄化(Rarefaction) num.resamp <- 1000 # 繰り返し数 ## Rarefaction for 新しい松林 out <- quantile(1, c(0.025, 0.5, 0.975)) for(i in 1:length(new)){ 
resamp <- sapply(1:num.resamp, function(x) length( levels( factor( sample(paddy, i) ) ) )) 
out <- rbind(out, quantile(resamp, c(0.025, 0.5, 0.975))) 
} 
## Rarefaction for 古い松林 out2 <- quantile(1, c(0.025, 0.5, 0.975)) for(i in 1:length(old)){ 
resamp <- sapply(1:num.resamp, function(x) length( levels( factor( sample(e, i) ) ) )) 
out2 <- rbind(out2, quantile(resamp, c(0.025, 0.5, 0.975))) 
} 
## 希薄化曲線の Plot epar <- par(no.readonly = TRUE) par(mar=c(5,5,1,1)) plot(out[,2], type="l", xlab="個体数", ylab="種数", xlim=c(0, 1300)) points(out[,1], type="l", lty="dashed") points(out[,3], type="l", lty="dashed") points(out2[,2], type="l", col="red") points(out2[,1], type="l", lty="dashed", col="red") points(out2[,3], type="l", lty="dashed", col="red") text(c(70, 250), c(6, 28), labels=c("江", "田"), col=c("red", "black") ) par(oldpar) 
