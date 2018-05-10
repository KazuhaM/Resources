#�p�b�P�|�W
library(mgcv)
library(ggplot2)
library(GGally)
library(MuMIn)

#�f�[�^�ǂݍ���
flname<-"SumCover"
e.data <- read.csv(paste(flname,".csv",sep=""),header = T)
head(e.data)

plot(e.data$ErosionPin~e.data$Cover)

#ggpairs(na.omit(e.data ), 
#        upper=list(continuous="smooth"),
#        params=list(corSize=6,labelSize=10)) #�v�f�Ԃ̑�������O���t��`��#

#GAMM
gam.model <- gam(ErosionPin~s(Cover), data=e.data,random =ID)    
summary(gam.model)
#GLM
lm.model <- gam(ErosionPin~Cover, data=e.data,random =ID)
summary(lm.model)

#GAMM���v���b�g
plot(gam.model, residuals=T, pch=1, se=T, main="Spline smoothing non Annual herbs",xlab="Cover(%)",ylab="Erosion Pin(cm)",
	cex.lab  = 1.5,       #  ���̐����̎��̑傫����ݒ肷��
     	cex.axis = 1.5,      #  ���̐������i���x���j�̑傫����ݒ肷��
     	cex.main = 1.8      #  ���C���^�C�g���̎��̑傫����ݒ肷��
)
	dev.copy(pdf, file=paste(flname,".pdf",sep=""), width = 10, height = 10)
	dev.off()
#GAMM�����Ă͂܂肷���Ă��Ȃ����`�F�b�N
gam.check(gam.model)

	dev.copy(pdf, file=paste(flname,"_Check.pdf",sep=""), width = 10, height = 10)
	dev.off()


# lm,GAMM�����̐}���쐬
en.data <- data.frame(Cover=seq(min(e.data$Cover), max(e.data$Cover), 0.1))
gam.pred <- predict(gam.model, en.data)
lm.pred <- predict(lm.model, en.data)

plot(
  e.data$ErosionPin ~ e.data$Cover,
  xlab="SumCover�@not Annual", ylab="ErosionPin(mm)", 
  main="Spline smoothing by GAMM ", cex.main=2, cex.lab=1.5
)
lines(lm.pred ~ as.matrix(en.data), col=2, lwd=2)
lines(gam.pred ~ as.matrix(en.data), col=2, lwd=2)
legend("topleft", lwd=2, col=c(2, 4), legend=c("���` ��A", "�������X�v���C��"))

#lm���AGAMM�̂ق����������Ɗm����
anova(lm.model, gam.model,test="F")

#GAMM�̌��₷���}
en.data <- data.frame(Cover=seq(min(e.data$Cover), max(e.data$Cover), 0.1))
gam.pred <- predict(gam.model, en.data, se.fit=TRUE, type="response")
lm.pred <- predict(lm.model, en.data, se.fit=TRUE, type="response")
critval=qnorm(0.975,0,1)
conf.lwr <- gam.pred$fit - critval* gam.pred$se.fit
conf.upr <- gam.pred$fit + critval* gam.pred$se.fit
gam.pred <- data.frame(en.data, 
                       as.data.frame(gam.pred), 
                       conf.lwr = conf.lwr, 
                       conf.upr = conf.upr )

ggplot(data=e.data) +
  geom_point(aes(x=Cover,y=ErosionPin), size=5) +
  geom_line(data=gam.pred, 
            aes(x=Cover, y=fit), size=1.2, colour="red")+
  geom_ribbon(data=gam.pred, 
              aes(x=Cover, ymin=conf.lwr, ymax=conf.upr),
              alpha=0.2, fill="red")+
  labs(x = "Cover", 
       y = "ErosionPin", size=2) +
  theme(axis.title.x = element_text(size=20),
        axis.title.y = element_text(size=20))


# �O�l�O�l�x�i�������p�����[�^�j�̊m�F
# sp��0.001����0.1�܂ŁA0.001���݂ŕω�������
sp <- seq(from=0, to=10, by=0.01)
 
# sp���ƂɁAGCV���v�Z
GCV <- numeric()
for(i in 1:length(sp)){
  g.m <- gam(ErosionPin~s(Cover), sp=sp[i], data=e.data)
  GCV[i] <- g.m$gcv.ubre
}
 
# sp��GCV�̊֌W���v���b�g
plot(GCV ~ sp, main="GCV�ƃO�l�O�l�x", cex.main=2)
 
# mgcv�֐��őI�΂ꂽsp���d�˂ăv���b�g����
points(gam.model$sp, gam.model$gcv.ubre, col=2, pch=18, cex=2)

