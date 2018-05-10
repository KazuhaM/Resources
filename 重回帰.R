read.csv("‰ðÍ—pcv.csv",header=T)->cv

fit.full<- glm(cv~Ž{HŒãŒo‰ß”N”+ŽÎ–ÊŒü‚«+”dŽí”+ŽÎ–ÊˆÊ’u, data=cv, family=gaussian)
fit.E_flower<- glm(Elymus_flower~Ž{HŒãŒo‰ß”N”+ŽÎ–ÊŒü‚«+”dŽí”+ŽÎ–ÊˆÊ’u, data=cv, family=poisson)
fit.A_flower<- glm(Agropyron_flower~Ž{HŒãŒo‰ß”N”+ŽÎ–ÊŒü‚«+”dŽí”+ŽÎ–ÊˆÊ’u, data=cv, family=poisson)
fit.all_flower<- glm(all_flowers~Ž{HŒãŒo‰ß”N”+ŽÎ–ÊŒü‚«+”dŽí”+ŽÎ–ÊˆÊ’u, data=cv, family=poisson)
fit.Ží”<- glm(Ží”~Ž{HŒãŒo‰ß”N”+ŽÎ–ÊŒü‚«+”dŽí”+ŽÎ–ÊˆÊ’u, data=cv, family=poisson)
fit.‘½—l“xŽw”<- glm(‘½—l“xŽw”~Ž{HŒãŒo‰ß”N”+ŽÎ–ÊŒü‚«+”dŽí”+ŽÎ–ÊˆÊ’u, data=cv, family=gaussian)
fit.E_h<- glm(Elymus_height~Ž{HŒãŒo‰ß”N”+ŽÎ–ÊŒü‚«+”dŽí”+ŽÎ–ÊˆÊ’u, data=cv, family=gaussian)
fit.C_h<- glm(Caragana_height~Ž{HŒãŒo‰ß”N”+ŽÎ–ÊŒü‚«+”dŽí”+ŽÎ–ÊˆÊ’u, data=cv, family=gaussian)



