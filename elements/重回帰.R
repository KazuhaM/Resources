read.csv("解析用cv.csv",header=T)->cv

fit.full<- glm(cv~施工後経過年数+斜面向き+播種数+斜面位置, data=cv, family=gaussian)
fit.E_flower<- glm(Elymus_flower~施工後経過年数+斜面向き+播種数+斜面位置, data=cv, family=poisson)
fit.A_flower<- glm(Agropyron_flower~施工後経過年数+斜面向き+播種数+斜面位置, data=cv, family=poisson)
fit.all_flower<- glm(all_flowers~施工後経過年数+斜面向き+播種数+斜面位置, data=cv, family=poisson)
fit.種数<- glm(種数~施工後経過年数+斜面向き+播種数+斜面位置, data=cv, family=poisson)
fit.多様度指数<- glm(多様度指数~施工後経過年数+斜面向き+播種数+斜面位置, data=cv, family=gaussian)
fit.E_h<- glm(Elymus_height~施工後経過年数+斜面向き+播種数+斜面位置, data=cv, family=gaussian)
fit.C_h<- glm(Caragana_height~施工後経過年数+斜面向き+播種数+斜面位置, data=cv, family=gaussian)



