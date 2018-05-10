speak_sex <- read.table("speak_sex.txt")
speak_sex
table(speak_sex)
man<-speak_sex[,2]==(M)
femail<-speak_sex[,F]
If?
help(If)
??If
result<-124/(108+124)-62/(116+62)
result
t.test(result)

if(speak_sex[,2]==M){}
femail

man<-which(speak_sex[,2]=="M")
man
femail<-which(speak_sex[,2]=="F")
femail
man1<-which(speak_sex[man,]=="1")
man1
man0<-which(speak_sex[man,]=="0")
man0
femail1<-which(speak_sex[femail,]=="1")
femail1
femail0<-which(speak_sex[femail,]=="0")
femail0
sex<-length(femail1)/length(femail)-length(man1)/length(man)
sex
sex.sd<-sd(sex)
sex.sd
man.diff<-length(man1)/length(man)
femail.diff<-length(femail1)/length(femail)
man.diff
femail.diff
t.test(man.diff,femail.diff, paired=T)











