library(dismo)
#?maxent

#file with presence points
occurence <- paste(system.file(package = "dismo"), '/ex/bradypus.csv', sep="")
occ <- read.table(occurence, header = T, sep = ",")[,-1]
head(occ)

#witholding a 20% sample for testing
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1,]
occtrain <- occ[fold != 1,]
fold

#get predictor variable
fnames <-list.files(path=paste(system.file(package="dismo"),'/ex',sep=""),pattern="grd",full.names=T)
fnames

predictors <- stack(fnames)
plot(predictors)

#testing
#background data
bg <- randomPoints(predictors, 1000)
head(bg)

#環境値の抽出
env_bg <- data.frame(extract(predictors, bg))
head(env_bg)
env_occ <-data.frame(extract(predictors, occ))
head(env_occ)

#replacing data for GLM
resp_occ <- rep(1, dim(occ)[1])
resp_bg <- rep(0,1000)
resp <- c(resp_occ,resp_bg)  #在・不在合わせて1116の応答変数
env <- rbind(env_occ, env_bg)  #在・不在合わせて1116地点、9項目の環境変数
dim(env)
length(resp)
cbind(resp, env) -> d_bradypus
head(d_bradypus)

#GLM
result.glm <- glm(resp ~ bio1+bio12+bio16+bio17+bio5+bio6+bio7+bio8, family = binomial, data = d_bradypus)
result.glm

#多重共線性のチェック、相関係数の計算のためにラスタースタックからデータフレームに変換
predictors.df <- as.data.frame(predictors, xy=T, na.rm = T)
summary(predictors.df)
predictors.df[,"biome"] <- as.factor(predictors.df[,"biome"])
summary(predictors.df)

#多重共線性のチェック
cor(predictors.df[,3:10])
pairs(predictors.df[,3:10], cex=0.2)

#主成分分析。第一主成分とかで高く寄与しているものは削りたくない。同じ主成分で寄与している者同士の中で削っていく
result.pca <- princomp(predictors.df[,3:10])
result.pca$loading

#強く相関のあるもののうち、気温同士、降水量同士を除いて再度
library(MASS)
glm.stepAIC <- glm(resp ~ ., family = binomial, data = d_bradypus[,c("resp","bio1","bio5","bio7","bio12","bio17")])
stepAIC(glm.stepAIC) -> result.stepAIC
summary(glm.stepAIC)
summary(result.stepAIC)

#二次の項まで考える
glm.stepAIC <- glm(resp ~ .^2, family = binomial, data = d_bradypus[,c("resp","bio1","bio5","bio7","bio12","bio17")])
stepAIC(glm.stepAIC) -> result.stepAIC

#推定に使った地点の推定結果を見てみる
pred_bradypus <- predict(result.stepAIC)
plot(pred_bradypus, d_bradypus[,"resp"],cex=0.5, pch=19)

#逆ロジット変換してみる（上のだと線形結合子で結果が表示されるから確率で表示したい。）
inverse_logit <- function(x) 1/(1+exp(-x))
plot(inverse_logit(pred_bradypus), d_bradypus[,"resp"])

#変数への応答
response(result.stepAIC)
#予測値を逆ロジット変換するように関数を指定
response(result.stepAIC, fun = function(x,y){inverse_logit(predict(x,y))})
#変数をどこで固定するか？デフォルトはメディアン
response(result.stepAIC, fun = function(x,y){inverse_logit(predict(x,y))},at = mean)

#ほかの地点の存在確率を予測してみる
pred_raster <- predict(predictors,result.stepAIC,
	fun = function(x,y){inverse_logit(predict(x,y))})
plot(pred_raster)
points(occ,cex=0.5,col="blue",pch=19)

#AUC：評価指標　クロスバリエーション分割してテストとモデル構築を行い評価
#occtrainでモデルを作る→モデルにocctestを入れる→実測値の環境データを用いて評価
#maxentでは自動でやってくれるが、glmでは自動でするものがあるはずだけで今わかんない→探せばあるはず。仕組みは以上の感じ



#RでMaxent
library(dismo)
library(biomod2)
Sys.setenv(JAVA_HOME = 'C:\\Program Files\\Java\\jdk1.8.0_92')
library(rJava)

help(maxent)
jar <- paste(system.file(package="dismo"), "/java/maxent.jar",sep="")
if (file.exists(jar) & require(rJava)){
 print("OK")
}

# get predictor variables
	fnames <- list.files(path=paste(system.file(package="dismo"),'/ex',sep=""),pattern='grd',full.names=T)
fnames
predictors <-stack(fnames) 
plot(predictors)
#在のみデータを読み込む
occurence <- paste(system.file(package = "dismo"), '/ex/bradypus.csv', sep="")
occ <- read.table(occurence, header = T, sep = ",")[,-1]
head(occ)
 
#witholding a 20% sample for testing
fold <- kfold(occ, k=5)
occtest <- occ[fold == 1,]
occtrain <- occ[fold != 1,]
fold

#fit model, biome is a categorical variable
		me <- maxent(predictors, occtrain, factors='biome',path="E:\\OneDrive - g.ecc.u-tokyo.ac.jp\\LEP\\Public in LEP\\university\\lecture\\修士\\生態統計学1\\report")
# see the maxent result in a browser;
me

# use "arges"
	me2<-maxent(predictors,occtrain,factors="biome",args=c("-J","-P"),path="E:\\OneDrive - g.ecc.u-tokyo.ac.jp\\LEP\\Public in LEP\\university\\lecture\\修士\\生態統計学1\\report")
me2
# plot showing important of each variable
	plot(me)
#response curves
	response(me)
# predict to entire dataset
	r<- predict(me,predictors)
	plot(r)
	points(occ)
# testing 
#background data
	bg<-randomPoints(predictors, 1000)
	head(bg)
#simplest way to use "evaluate"
	#el<-dismo::evaluate(me,p=occtest, a=bg, x=predictors)
	el<-dismo::evaluate(me,p=occtest, a=bg)
	el
# predict to entire dataset
	r<- predict(me,predictors)
	plot(r)
	points(occ)

#データフレームで環境値を与える
#alternative 1
#extract values
	pvtest<-data.frame(extract(predictors, occtest))
	avtest<-data.frame(extract(predictors, bg))
	head(pvtest)
	head(avtest)
	e2<-dismo::evaluate(me,p=pvtest,a=avtest)
e2

#テスト用の点での予測値を与えて評価
#predict to testing points
	testp<-predict(me,pvtest)
	head(testp)
	e3<-dismo::evaluate(p=testp,a=testa)
e3
	threshold(e3)
	plot(e3,"ROC")
