library(rjags)
n <- 100
y <- 25
BL <- function(p) choose(n,y)*p^(y)*(1-p)^(n-y)
plot(0:100/100, BL(0:100/100), type="l")
#���O2 �s�́A�ȉ��̊֐����g���Ă��`����
#plot(0:100, dbinom(0:100, n, y/n), type="l")
abline(v=y/n, lwd=2)
text(y/n+0.05,0.09,labels="0.25")

####################
#�������v�w�̎��s�R�[�h
#�쐬�ҁF�ѓ��E�l�i�R�����X�ь��j
#�A����Fhayato.iijima@gmail.com
#�쐬���F2017/1/16
####################

####################
#2�́F�Ȃ����v�w���K�v�Ȃ̂��H
####################
##���K���z
#�O���t�B�b�N�X�p�����[�^�̒�`
par(mar=c(5,5,1,1), ps=15)
#�`��
values <- -50:50
plot(values, dnorm(values, 0, 1), type="l", xlab="Value", ylab="Probability density")
points(values, dnorm(values, 0, 5), col="red", type="l")
points(values, dnorm(values, 0, 10), col="blue", type="l")
points(values, dnorm(values, 0, sqrt(10^3)), col="green", type="l")
points(values, dnorm(values, -20, 3), type="l", lty=2)
points(values, dnorm(values, 40, 6), type="l", lty=2)
legend("topleft", lty=c(rep(1, 4), 2, 2), col=c("black", "red", "blue", "green", "black", "black"),
       legend=c(expression(paste(mu, " = 0, ", sigma, " = 1")),
                expression(paste(mu, " = 0, ", sigma, " = 5")),
		expression(paste(mu, " = 0, ", sigma, " = 10")),
		expression(paste(mu, " = 0, ", sigma, " = 31.6")),
		expression(paste(mu, " = -20, ", sigma, " = 3")),
		expression(paste(mu, " = 40, ", sigma, " = 6")))
)

###�񍀕��z
##�O���t�B�b�N�X�p�����[�^�̒�`
par(mfrow=c(1,2), mar=c(5,5,1,1), ps=15)
##���N�m���̈Ⴂ
#���N�m��
prob <- c(0.05, 0.3, 0.5, 0.8)
#���s��
N <- 100
#�F�̐ݒ�
iro <- c("black", "red", "blue", "green")
#�`��
plot(0:N, dbinom(0:N, N, prob[1]), type="h", xlab="Value", ylab="Probability")
points(0:N, dbinom(0:N, N, prob[2]), type="h", col=iro[2])
points(0:N, dbinom(0:N, N, prob[3]), type="h", col=iro[3])
points(0:N, dbinom(0:N, N, prob[4]), type="h", col=iro[4])
legend("topright", lty=1, col=iro,
       legend=c("p = 0.05",
                "p = 0.3",
		"p = 0.5",
		"p = 0.8"
       ), cex=0.7
)
##���s�񐔂̈Ⴂ
#���N�m��
prob <- 0.5
#���s�񐔁i�����ĕs���m�Ȏ����������܂��j
N <- c(100, 30, 20, 10)
#�`��
plot(0:N[1], dbinom(0:N[1], N[1], prob), type="h", xaxt="n", xlab="Relative position", ylab="Probability")
axis(1, 0:5*20, at=0:5*20, labels=0:5/5, tick=TRUE)
for (i in 2:4) {
     par(new=TRUE)
     plot(0:N[i], dbinom(0:N[i], N[i], prob), type="h", axes=F, ann=F, col=iro[i])
}
legend("topright", lty=1, col=iro,
       legend=c("N = 100",
                "N = 30",
		"N = 20",
		"N = 10"
       ), cex=0.7
)


##�|�A�\�����z
#���ς̔�����
N <- c(1, 3, 7, 15)
values <- 0:30
#�F�̐ݒ�
iro <- c("black", "red", "blue", "green")
#�`��
plot(values, dpois(values, N[1]), type="h", xlab="Values", ylab="Probability")
for (i in 2:4) {
     points(jitter(values), dpois(values, N[i]), type="h", col=iro[i])
}
legend("topright", lty=1, col=iro,
       legend=c(expression(paste(lambda, " = 1")),
                expression(paste(lambda, " = 3")),
		expression(paste(lambda, " = 7")),
		expression(paste(lambda, " = 15")))
)




##############
#4�́FGLM�i��ʉ����^���f���j
##############
#�f�[�^�̐���<-�f�[�^�͍���͗����Ő���
set.seed(1)
N <- 100
x1 <- rnorm(N, 0, 2)
x2 <- rnorm(N, 0, 2)
intercept <- -2
y <- rpois(N, exp(intercept + x1)) #x2�͉e�����Ȃ�
d <- data.frame(y, x1, x2)
head(d)
,
#�f�[�^���m�̊֌W
#�O���t�B�b�N�X�p�����[�^�̒�`
par(mfrow=c(1,2), mar=c(5,5,1,1), ps=15)
#�`��
plot(y ~ x1, d)
plot(y ~ x2, d)


#glm()�ɂ����
res <- glm(y ~ x1 + x2, family=poisson(link="log"), data = d)
summary(res)

library(MASS)
stepAIC(res)

#JAGS��p�������
#�f�[�^��p�ӂ���
list.data <- list(N=N, x1=x1, x2=x2, y=y)
#���f����ǂݍ��܂��A�e�L�X�g�t�@�C���Ƃ��ďo�͂���
modelFilename = "testmod.txt"
cat("

#BUGS����Ń��f�����L�q����
model {
#�l�������f��
for (i in 1:N) {
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- intercept + bx1*x1[i] + bx2*x2[i]
    # lambda[i] <- exp(intercept + bx1*x1[i] + bx2*x2[i])�Ƃ��Ă����� 
}
#�p�����[�^�̎��O���z
intercept ~ dnorm(0.0, 1.0E-3)
bx1 ~ dnorm(0.0, 1.0E-3)
bx2 ~ dnorm(0.0, 1.0E-3)
} #���f���̋L�q�͂����܂�
", fill=TRUE, file=modelFilename)

#�����l��^����
inits <- list(intercept = 0,
                 bx1 = 0,
                 bx2 = 0
)
inits <- list(inits, inits, inits)
#�����l�̗����̎�ɈقȂ�l��^����i�����͉��ł������ł��j
inits[[1]]$.RNG.name <- "base::Mersenne-Twister"
inits[[1]]$.RNG.seed <- 1
inits[[2]]$.RNG.name <- "base::Mersenne-Twister"
inits[[2]]$.RNG.seed <- 12
inits[[3]]$.RNG.name <- "base::Mersenne-Twister"
inits[[3]]$.RNG.seed <- 123

#�Ď��Ώۃp�����[�^��ݒ肷��
para <- c("intercept", "bx1", "bx2")


#JAGS�ɂ��v�Z�̎��s
#MCMC�̌v�Z�Ɋւ���p�����[�^
n.chains <- 3
n.iter <- 2000
n.update <- 2000
thin <- 2

#�v�Z�ɕK�v�ȃp�b�P�[�W�̓ǂݍ���
library(rjags)

#�v�Z�J�n���Ԃ��L�^
start.time <- Sys.time()
#�������i�G���[������ꍇ�͂����ŏo�܂��j
m <- jags.model(
	file = modelFilename,
	data = list.data,
	inits = inits,
	n.chain = n.chains
)
#Burn-in�̎��s
update(m, n.update)
#�{�v�Z�̎��s
x <- coda.samples(
        m,
        para,
        thin = thin, n.iter = n.iter
)
#�I�����Ԃ̋L�^�ƁA�v�Z���Ԃ̏o��
end.time <- Sys.time()
elapsed.time <- difftime(end.time, start.time, units='hours')
cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' hours\n', sep=''))

#�v�Z���ʂ̏o��
res <- data.frame(summary(x)$statistics)
ci <- data.frame(summary(x)$quantiles)
#95%�M�p��Ԃ�0���܂������ǂ������v�Z
res$sig <- abs(sign(ci[, 1]) + sign(ci[, 5])) == 2
#Rhat�l�̌v�Z
rhat <- gelman.diag(x)[["psrf"]][, 1]
res$Rhat <- rhat
#���ʂ̕\��
res
plot(x)






##############
#5�́FGLMM�i��ʉ����`�������f���j
##############
#�f�[�^�̐���
set.seed(11)
N <- 100
x1 <- rnorm(N, 0, 2)
x2 <- rnorm(N, 0, 2)
intercept <- -2
sigma <- 2
Nplot <- 10
plot <- rnorm(Nplot, 0, sigma)
plot <- rep(plot, c(10, 10, 4, 11, 7, 14, 8, 18,  11,  7))
plotid <- rep(1:10, c(10, 10, 4, 11, 7, 14, 8, 18,  11,  7))
y <- rpois(N, exp(intercept + x1 + plot)) #x2�͉e�����Ȃ�
d2 <- data.frame(y, x1, x2, plot, plotid)

#glmmML()�ɂ����
library(glmmML)
res2 <- glmmML(y ~ x1 + x2, cluster=plotid, family=poisson, d2)

#JAGS�ɂ����
#�f�[�^��p�ӂ���
list.data <- list(N=N, x1=x1, x2=x2, y=y, plotid=plotid, Nplot=Nplot)
#���f����ǂݍ��܂��A�e�L�X�g�t�@�C���Ƃ��ďo�͂���
modelFilename = "testmod.txt"
cat("

#BUGS����Ń��f�����L�q����
model {
#�l�������f��
for (i in 1:N) {
    y[i] ~ dpois(lambda[i])
    log(lambda[i]) <- intercept + bx1*x1[i] + bx2*x2[i] + ranef[plotid[i]]
}
#�p�����[�^�̎��O���z
intercept ~ dnorm(0.0, 1.0E-3)
bx1 ~ dnorm(0.0, 1.0E-3)
bx2 ~ dnorm(0.0, 1.0E-3)
for (i in 1:Nplot) {
    ranef[i] ~ dnorm(0.0, tau)
}
tau <- pow(sigma, -2)
sigma ~ dunif(0, 100)
} #���f���̋L�q�͂����܂�
", fill=TRUE, file=modelFilename)

#
#�����l��^����
inits <- list(intercept = 0,
                 bx1 = 0,
                 bx2 = 0,
                 ranef = rnorm(Nplot, 0, 1),
                 sigma = 5
)
inits <- list(inits, inits, inits)
#�����l�̗����̎�ɈقȂ�l��^����
inits[[1]]$.RNG.name <- "base::Mersenne-Twister"
inits[[1]]$.RNG.seed <- 1
inits[[2]]$.RNG.name <- "base::Mersenne-Twister"
inits[[2]]$.RNG.seed <- 12
inits[[3]]$.RNG.name <- "base::Mersenne-Twister"
inits[[3]]$.RNG.seed <- 123

#�Ď��Ώۃp�����[�^��ݒ肷��
para <- c("intercept", "bx1", "bx2", "ranef", "sigma")

#JAGS�ɂ��v�Z�̎��s
#MCMC�̌v�Z�Ɋւ���p�����[�^
n.chains <- 3
n.iter <- 2000
n.update <- 2000
thin <- 2

#�v�Z�ɕK�v�ȃp�b�P�[�W�̓ǂݍ���
library(rjags)

#�v�Z�J�n���Ԃ��L�^
start.time <- Sys.time()
#������
m <- jags.model(
	file = modelFilename,
	data = list.data,
	inits = inits,
	n.chain = n.chains
)
#Burn-in�̎��s
update(m, n.update)
#�{�v�Z�̎��s
x <- coda.samples(
        m,
        para,
        thin = thin, n.iter = n.iter
)
#�I�����Ԃ̋L�^�ƁA�v�Z���Ԃ̏o��
end.time <- Sys.time()
elapsed.time <- difftime(end.time, start.time, units='hours')
cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' hours\n', sep=''))

#�v�Z���ʂ̏o��
res <- data.frame(summary(x)$statistics)
ci <- data.frame(summary(x)$quantiles)
#95%�M�p��Ԃ�0���܂������ǂ������v�Z
res$sig <- abs(sign(ci[, 1]) + sign(ci[, 5])) == 2
#Rhat�l�̌v�Z
rhat <- gelman.diag(x)[["psrf"]][, 1]
res$Rhat <- rhat
#���ʂ̕\��
res

#�����_�����ʂ̎��㕪�z
par(ask=TRUE)
plot(x[, grep("ranef", rownames(res))])









##############
#6�́F��ԋ�ԃ��f��
##############
#�f�[�^�̐���
set.seed(111)
Nyear <- 25
Nint <- 30.5
mean.lambda <- 1.02
sigma.lambda <- 0.1
sigma.obs <- 5
N <- as.numeric()
y <- as.numeric()
N[1] <- Nint
lambda <- rnorm(Nyear-1, mean.lambda, sigma.lambda)
for (i in 1:(Nyear-1)) {
      N[i+1] <- lambda[i]*N[i]
}
for (i in 1:Nyear) {
      y[i] <- rnorm(1, N[i], sigma.obs)
}
Ntau <- 2
d3 <- data.frame(year = 1:Nyear, N, y)

#
#�f�[�^��p�ӂ���
list.data <- list(y=y, Nyear=Nyear, Ntau=Ntau)
#���f����ǂݍ��܂��A�e�L�X�g�t�@�C���Ƃ��ďo�͂���
modelFilename = "testmod.txt"
cat("

#BUGS����Ń��f�����L�q����
model {
#��ԃv���Z�X
for (i in 1:(Nyear-1)) {
      N[i+1] <- lambda[i]*N[i]
      lambda[i] ~ dnorm(mulambda, tau[1])
}
N[1] <- Ninit
Ninit ~ dnorm(0.0, 1.0E-3)
mulambda ~ dnorm(0.0, 1.0E-3)

#�ϑ��v���Z�X
for (i in 1:Nyear) {
      y[i] ~ dnorm(N[i], tau[2])
}
#�p�����[�^�̎��O���z
for (i in 1:Ntau) {
      tau[i] <- pow(sigma[i], -2)
      sigma[i] ~ dunif(0, 100)
}
} #���f���̋L�q�͂����܂�
", fill=TRUE, file=modelFilename)


#
#�����l��^����
inits <- list(lambda = rnorm(Nyear-1, 1.1, 0.1),
                 mulambda = 0,
                 Ninit = 30,
                 sigma = rep(5, Ntau)
)
inits <- list(inits, inits, inits)
#�����l�̗����̎�ɈقȂ�l��^����
inits[[1]]$.RNG.name <- "base::Mersenne-Twister"
inits[[1]]$.RNG.seed <- 1
inits[[2]]$.RNG.name <- "base::Mersenne-Twister"
inits[[2]]$.RNG.seed <- 12
inits[[3]]$.RNG.name <- "base::Mersenne-Twister"
inits[[3]]$.RNG.seed <- 123

#�Ď��Ώۃp�����[�^��ݒ肷��
para <- c("N", "lambda", "mulambda", "sigma")

#JAGS�ɂ��v�Z�̎��s
#MCMC�̌v�Z�Ɋւ���p�����[�^
n.chains <- 3
n.iter <- 5000
n.update <- 5000
thin <- 5

#�v�Z�ɕK�v�ȃp�b�P�[�W�̓ǂݍ���
library(rjags)

#�v�Z�J�n���Ԃ��L�^
start.time <- Sys.time()
#������
m <- jags.model(
	file = modelFilename,
	data = list.data,
	inits = inits,
	n.chain = n.chains
)
#Burn-in�̎��s
update(m, n.update)
#�{�v�Z�̎��s
x <- coda.samples(
        m,
        para,
        thin = thin, n.iter = n.iter
)
#�I�����Ԃ̋L�^�ƁA�v�Z���Ԃ̏o��
end.time <- Sys.time()
elapsed.time <- difftime(end.time, start.time, units='hours')
cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' hours\n', sep=''))

#�v�Z���ʂ̏o��
res <- data.frame(summary(x)$statistics)
ci <- data.frame(summary(x)$quantiles)
#95%�M�p��Ԃ�0���܂������ǂ������v�Z
res$sig <- abs(sign(ci[, 1]) + sign(ci[, 5])) == 2
#Rhat�l�̌v�Z
rhat <- gelman.diag(x)[["psrf"]][, 1]
res$Rhat <- rhat
#���ʂ̕\��
res

#���ʂ̐}��
par(mfrow=c(2,2), mar=c(5,5,3,1),ps=15)
#���x
plot(1:Nyear, N, type="l", lwd=2, ylim=c(0, 100))
lines(1:Nyear, y, col="red")
lines(1:Nyear, res[grep("N", rownames(res)), 1], col="blue")
legend("topright", col=c("black", "red", "blue"), lwd=c(2,1,1),
            legend=c("Setting", "Observed", "Estimated"), cex=0.6
)
#�̌Q�������̕��ϒl
plot(density(unlist(x[, grep("mulambda", rownames(res))])), main="mean.lambda")
abline(v=mean.lambda, lwd=3, lty=2, col="red")
#�̌Q�������̕W���덷
plot(density(unlist(x[, grep("sigma", rownames(res))[1]])), main="sigma.lambda")
abline(v=sigma.lambda, lwd=3, lty=2, col="red")
#�ϑ��덷
plot(density(unlist(x[, grep("sigma", rownames(res))[2]])), main="sigma.obs")
abline(v=sigma.obs, lwd=3, lty=2, col="red")
legend("topright", lty=c(1,2), lwd=c(1,3), col=c("black", "red"),
            legend=c("Estimated", "Setting"), cex=0.6
)







##############
#7�́F��ԋ�ԃ��f���i�񍀍������f���j
##############
#�f�[�^�̐���
set.seed(11111)
Nsite <- 20
Nrep <- 2
meanlogitp <- log(0.7/(1-0.7))
light <- rnorm(Nsite, 0, 0.5)
coefL <- 2
p <- 1/(1+exp(-(meanlogitp + coefL*light)))
lambda <- 5
N <- rpois(Nsite, lambda)
y <- matrix(NA, nrow=Nrep, ncol=Nsite)
for (i in 1:Nsite) {
     y[, i] <- rbinom(Nrep, N[i], p[i])
}
#���������f�[�^������
par(mar=c(5,5,1,1), ps=15)
plot(1:length(N), N, pch=15, xlab="Site", ylab="Abundance", ylim=c(0, max(N)), cex=2)
points(1:length(N), y[1, ], pch=1, cex=2)
points(1:length(N), y[2, ], pch=2, cex=2)
legend("topright", pch=c(15, 1, 2),
       legend=c("True", "Obs. 1", "Obs. 2"))

#���������
#�f�[�^��p�ӂ���
list.data <- list(y=y, Nsite=Nsite, Nrep=Nrep)
#���f����ǂݍ��܂��A�e�L�X�g�t�@�C���Ƃ��ďo�͂���
modelFilename = "testmod.txt"
cat("

#BUGS����Ń��f�����L�q����
model {
#��ԃv���Z�X
for (i in 1:Nsite) {
     EN[i] ~ dpois(meanN)
}
meanN ~ dunif(0, 1000)

#�ϑ��v���Z�X
for (i in 1:Nsite) {
     for (j in 1:Nrep) {
          y[j, i] ~ dbin(estp, EN[i])
     }
}
estp ~ dunif(0, 1)
} #���f���̋L�q�͂����܂�
", fill=TRUE, file=modelFilename)

#�����l��^����
inits <- list(EN = N,
              meanN = 5,
	      estp = 0.5
)
inits <- list(inits, inits, inits)
#�����l�̗����̎�ɈقȂ�l��^����
inits[[1]]$.RNG.name <- "base::Mersenne-Twister"
inits[[1]]$.RNG.seed <- 1
inits[[2]]$.RNG.name <- "base::Mersenne-Twister"
inits[[2]]$.RNG.seed <- 12
inits[[3]]$.RNG.name <- "base::Mersenne-Twister"
inits[[3]]$.RNG.seed <- 123

#�Ď��Ώۃp�����[�^��ݒ肷��
para <- c("EN", "meanN", "estp")

#JAGS�ɂ��v�Z�̎��s
#MCMC�̌v�Z�Ɋւ���p�����[�^
n.chains <- 3
n.iter <- 10000
n.update <- 10000
thin <- 5

#�v�Z�ɕK�v�ȃp�b�P�[�W�̓ǂݍ���
library(rjags)

#�v�Z�J�n���Ԃ��L�^
start.time <- Sys.time()
#������
m <- jags.model(
	file = modelFilename,
	data = list.data,
	inits = inits,
	n.chain = n.chains
)
#Burn-in�̎��s
update(m, n.update)
#�{�v�Z�̎��s
x <- coda.samples(
        m,
        para,
        thin = thin, n.iter = n.iter
)
#�I�����Ԃ̋L�^�ƁA�v�Z���Ԃ̏o��
end.time <- Sys.time()
elapsed.time <- difftime(end.time, start.time, units='hours')
cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' hours\n', sep=''))

#���ʂ̏o��
res <- data.frame(summary(x)$statistics)
ci <- data.frame(summary(x)$quantiles)
#95%�M�p��Ԃ�0���܂������ǂ������v�Z
res$sig <- abs(sign(ci[, 1]) + sign(ci[, 5])) == 2
#Rhat�l�̌v�Z
rhat <- gelman.diag(x)[["psrf"]][, 1]
res$Rhat <- rhat
#���ʂ̕\��
res

#�ݒ�l�Ƃ̊֌W
par(mfrow=c(1,2), mar=c(5,5,1,1), ps=15)
#�^�̌̐�
plot(N, res[grep("EN", rownames(res)), 1], xlab="Set", ylab="Estimated (mean)",
     main="Abundance", xlim=c(0, max(N)+2), ylim=c(0, max(N)+2))
abline(a=0, b=1, lwd=2)
#���ς̌̐�
plot(density(unlist(x[, grep("meanN", rownames(res))])), main=expression(lambda))
abline(v=lambda)

#���o���ϓ�
#�f�[�^��p�ӂ���
list.data <- list(y=y, Nsite=Nsite, Nrep=Nrep, light=light)
#���f����ǂݍ��܂��A�e�L�X�g�t�@�C���Ƃ��ďo�͂���
modelFilename = "testmod.txt"
cat("

#BUGS����Ń��f�����L�q����
model {
#��ԃv���Z�X
for (i in 1:Nsite) {
     EN[i] ~ dpois(meanN)
}
meanN ~ dunif(0, 1000)

#�ϑ��v���Z�X
for (i in 1:Nsite) {
     for (j in 1:Nrep) {
          y[j, i] ~ dbin(estp[i], EN[i])
     }
     estp[i] <- 1/(1+exp(-(alpha + bLI*light[i])))
}
alpha ~ dnorm(0.0, 1.0E-3)
bLI ~ dnorm(0.0, 1.0E-3)
} #���f���̋L�q�͂����܂�
", fill=TRUE, file=modelFilename)


#
#�����l��^����
inits <- list(EN = N,
              meanN = 5,
	      bLI = 0,
	      alpha = 0
)
inits <- list(inits, inits, inits)
#�����l�̗����̎�ɈقȂ�l��^����
inits[[1]]$.RNG.name <- "base::Mersenne-Twister"
inits[[1]]$.RNG.seed <- 1
inits[[2]]$.RNG.name <- "base::Mersenne-Twister"
inits[[2]]$.RNG.seed <- 12
inits[[3]]$.RNG.name <- "base::Mersenne-Twister"
inits[[3]]$.RNG.seed <- 123

#�Ď��Ώۃp�����[�^��ݒ肷��
para <- c("EN", "meanN", "alpha", "bLI")

#JAGS�ɂ��v�Z�̎��s
#MCMC�̌v�Z�Ɋւ���p�����[�^
n.chains <- 3
n.iter <- 10000
n.update <- 10000
thin <- 5

#�v�Z�ɕK�v�ȃp�b�P�[�W�̓ǂݍ���
library(rjags)

#�v�Z�J�n���Ԃ��L�^
start.time <- Sys.time()
#������
m <- jags.model(
	file = modelFilename,
	data = list.data,
	inits = inits,
	n.chain = n.chains
)
#Burn-in�̎��s
update(m, n.update)
#�{�v�Z�̎��s
x2 <- coda.samples(
        m,
        para,
        thin = thin, n.iter = n.iter
)
#�I�����Ԃ̋L�^�ƁA�v�Z���Ԃ̏o��
end.time <- Sys.time()
elapsed.time <- difftime(end.time, start.time, units='hours')
cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' hours\n', sep=''))

#���ʂ̏o��
res2 <- data.frame(summary(x2)$statistics)
ci2 <- data.frame(summary(x2)$quantiles)
#95%�M�p��Ԃ�0���܂������ǂ������v�Z
res2$sig <- abs(sign(ci2[, 1]) + sign(ci2[, 5])) == 2
#Rhat�l�̌v�Z
rhat2 <- gelman.diag(x2)[["psrf"]][, 1]
res2$Rhat <- rhat2
#���ʂ̕\��
res2

#�ݒ�l�Ƃ̊֌W�i���o����胂�f���Ƃ̔�r�j
par(mfrow=c(2,2), mar=c(5,5,1,1), ps=15)
#���o�����
#�^�̌̐�
plot(N, res[grep("EN", rownames(res)), 1], xlab="Set", ylab="Estimated (mean)",
     main="Abundance", xlim=c(0, max(N)+2), ylim=c(0, max(N)+2))
abline(a=0, b=1, lwd=2)
#���ς̌̐�
plot(density(unlist(x[, grep("meanN", rownames(res))])), main=expression(lambda))
abline(v=lambda)
#���o���ϓ��i����̉�́j
plot(N, res2[grep("EN", rownames(res2)), 1], xlab="Set", ylab="Estimated (mean)",
     main="Abundance", xlim=c(0, max(N)+2), ylim=c(0, max(N)+2))
abline(a=0, b=1, lwd=2)
#���ς̌̐�
plot(density(unlist(x2[, grep("meanN", rownames(res2))])), main=expression(lambda))
abline(v=lambda)




#Bayesian p value�̌v�Z
#���o�����
#�f�[�^��p�ӂ���
list.data <- list(y=y, Nsite=Nsite, Nrep=Nrep)
#���f����ǂݍ��܂��A�e�L�X�g�t�@�C���Ƃ��ďo�͂���
modelFilename = "testmod.txt"
cat("

#BUGS����Ń��f�����L�q����
model {
#��ԃv���Z�X
for (i in 1:Nsite) {
     EN[i] ~ dpois(meanN)
}
meanN ~ dunif(0, 1000)

#�ϑ��v���Z�X
for (i in 1:Nsite) {
     for (j in 1:Nrep) {
          y[j, i] ~ dbin(estp, EN[i])
	  #���Ғl
	  esty[j, i] <- estp*EN[i]
	  E[j, i] <- pow((y[j, i] - esty[j, i]), 2)/(esty[j, i] + 0.5)
     }
}
estp ~ dunif(0, 1)
#Bayesian p value�v�Z
for (i in 1:Nsite) {
     for (j in 1:Nrep) {
          y_new[j, i] ~ dbin(estp, EN[i])
	  esty_new[j, i] <- estp*EN[i]
	  E_new[j, i] <- pow((y_new[j, i] - esty_new[j, i]), 2)/(esty_new[j, i] + 0.5)
     }
}
fit_data <- sum(E[, ])
fit_new <- sum(E_new[, ])
} #���f���̋L�q�͂����܂�
", fill=TRUE, file=modelFilename)

#�����l��^����
inits <- list(EN = N,
              meanN = 5,
	      y_new = y,
	      estp = 0.5
)
inits <- list(inits, inits, inits)
#�����l�̗����̎�ɈقȂ�l��^����
inits[[1]]$.RNG.name <- "base::Mersenne-Twister"
inits[[1]]$.RNG.seed <- 1
inits[[2]]$.RNG.name <- "base::Mersenne-Twister"
inits[[2]]$.RNG.seed <- 12
inits[[3]]$.RNG.name <- "base::Mersenne-Twister"
inits[[3]]$.RNG.seed <- 123

#�Ď��Ώۃp�����[�^��ݒ肷��
para <- c("EN", "meanN", "estp", "fit_data", "fit_new")

#JAGS�ɂ��v�Z�̎��s
#MCMC�̌v�Z�Ɋւ���p�����[�^
n.chains <- 3
n.iter <- 10000
n.update <- 10000
thin <- 5

#�v�Z�ɕK�v�ȃp�b�P�[�W�̓ǂݍ���
library(rjags)

#�v�Z�J�n���Ԃ��L�^
start.time <- Sys.time()
#������
m <- jags.model(
	file = modelFilename,
	data = list.data,
	inits = inits,
	n.chain = n.chains
)
#Burn-in�̎��s
update(m, n.update)
#�{�v�Z�̎��s
x <- coda.samples(
        m,
        para,
        thin = thin, n.iter = n.iter
)
#�I�����Ԃ̋L�^�ƁA�v�Z���Ԃ̏o��
end.time <- Sys.time()
elapsed.time <- difftime(end.time, start.time, units='hours')
cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' hours\n', sep=''))

#���ʂ̏o��
res <- data.frame(summary(x)$statistics)
ci <- data.frame(summary(x)$quantiles)
#95%�M�p��Ԃ�0���܂������ǂ������v�Z
res$sig <- abs(sign(ci[, 1]) + sign(ci[, 5])) == 2
#Rhat�l�̌v�Z
rhat <- gelman.diag(x)[["psrf"]][, 1]
res$Rhat <- rhat


#���o���ϓ�
#�f�[�^��p�ӂ���
list.data <- list(y=y, Nsite=Nsite, Nrep=Nrep, light=light)
#���f����ǂݍ��܂��A�e�L�X�g�t�@�C���Ƃ��ďo�͂���
modelFilename = "testmod.txt"
cat("

#BUGS����Ń��f�����L�q����
model {
#��ԃv���Z�X
for (i in 1:Nsite) {
     EN[i] ~ dpois(meanN)
}
meanN ~ dunif(0, 1000)

#�ϑ��v���Z�X
for (i in 1:Nsite) {
     for (j in 1:Nrep) {
          y[j, i] ~ dbin(estp[i], EN[i])
	  #���Ғl
	  esty[j, i] <- estp[i]*EN[i]
	  E[j, i] <- pow((y[j, i] - esty[j, i]), 2)/(esty[j, i] + 0.5)
     }
     estp[i] <- 1/(1+exp(-(alpha + bLI*light[i])))
}
alpha ~ dnorm(0.0, 1.0E-3)
bLI ~ dnorm(0.0, 1.0E-3)

#Bayesian p value�v�Z
for (i in 1:Nsite) {
     for (j in 1:Nrep) {
          y_new[j, i] ~ dbin(estp[i], EN[i])
	  esty_new[j, i] <- estp[i]*EN[i]
	  E_new[j, i] <- pow((y_new[j, i] - esty_new[j, i]), 2)/(esty_new[j, i] + 0.5)
     }
}
fit_data <- sum(E[, ])
fit_new <- sum(E_new[, ])
} #���f���̋L�q�͂����܂�
", fill=TRUE, file=modelFilename)

#�����l��^����
inits <- list(EN = N,
              meanN = 5,
	      y_new = y,
	      bLI = 0,
	      alpha = 0
)
inits <- list(inits, inits, inits)
#�����l�̗����̎�ɈقȂ�l��^����
inits[[1]]$.RNG.name <- "base::Mersenne-Twister"
inits[[1]]$.RNG.seed <- 1
inits[[2]]$.RNG.name <- "base::Mersenne-Twister"
inits[[2]]$.RNG.seed <- 12
inits[[3]]$.RNG.name <- "base::Mersenne-Twister"
inits[[3]]$.RNG.seed <- 123

#�Ď��Ώۃp�����[�^��ݒ肷��
para <- c("EN", "meanN", "alpha", "bLI", "fit_data", "fit_new")

#JAGS�ɂ��v�Z�̎��s
#MCMC�̌v�Z�Ɋւ���p�����[�^
n.chains <- 3
n.iter <- 10000
n.update <- 10000
thin <- 5

#�v�Z�ɕK�v�ȃp�b�P�[�W�̓ǂݍ���
library(rjags)

#�v�Z�J�n���Ԃ��L�^
start.time <- Sys.time()
#������
m <- jags.model(
	file = modelFilename,
	data = list.data,
	inits = inits,
	n.chain = n.chains
)
#Burn-in�̎��s
update(m, n.update)
#�{�v�Z�̎��s
x2 <- coda.samples(
        m,
        para,
        thin = thin, n.iter = n.iter
)
#�I�����Ԃ̋L�^�ƁA�v�Z���Ԃ̏o��
end.time <- Sys.time()
elapsed.time <- difftime(end.time, start.time, units='hours')
cat(paste(paste('Posterior computed in ', elapsed.time, sep=''), ' hours\n', sep=''))

#���ʂ̏o��
res2 <- data.frame(summary(x2)$statistics)
ci2 <- data.frame(summary(x2)$quantiles)
#95%�M�p��Ԃ�0���܂������ǂ������v�Z
res2$sig <- abs(sign(ci2[, 1]) + sign(ci2[, 5])) == 2
#Rhat�l�̌v�Z
rhat2 <- gelman.diag(x2)[["psrf"]][, 1]
res2$Rhat <- rhat2

#Bayesian p value�̌v�Z
#���o�����
mean(unlist(x[grep("fit_data", rownames(res)), ]) > unlist(x[grep("fit_new", rownames(res)), ]))
#���o���ϓ�
mean(unlist(x2[grep("fit_data", rownames(res2)), ]) > unlist(x2[grep("fit_new", rownames(res2)), ]))

