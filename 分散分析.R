read.csv("分散分析-経年変化.csv",header=T)->経年
summary(aov(Caragana_height~site*location,data=経年))
TukeyHSD(aov(Caragana_height~site*location,data=経年))


read.csv("分散分析-斜面向き.csv",header=T)->斜面
summary(aov(Elymus_height~斜面向き*斜面位置,data=斜面))
TukeyHSD(aov(Elymus_height~斜面向き*斜面位置,data=斜面))

read.csv("分散分析-手法.csv",header=T)->手法
summary(aov(Caragana_height~播種数*斜面位置,data=手法))
TukeyHSD(aov(Caragana_height~播種数*斜面位置,data=手法))

read.csv("解析用cv.csv",header=T)->進入
summary(aov(Caragana_number~site*location,data=進入))
TukeyHSD(aov(Caragana_number~site*location,data=進入))


2015

read.csv("2015R用.csv",header=T)->解析
summary(aov(cv~year*direction+Error(location+location:year+location:direction+location:year:direction),data=解析))->結果
