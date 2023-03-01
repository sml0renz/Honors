
library(VGAM)
source("http://rfs.kvasaheim.com/rfs.R")




set.seed(13)

############################ OLS  ##########################################

modA <- lm(1/log(time) ~ wl + sl + wn + sn)
eA <- residuals(modA)

hist(eA)
overlay(eA)
shapiroTest(eA)
plot(wl, exp(1/time))
bptest(modA)
runs.test(eA, order=wl)

summary(modA)
coef(modA)
1/exp(14.9642)
1/exp(1.4726)
1/exp(-1.7690)
1/exp(-0.8186)
1/exp(-0.2244)

##################################### GLM ###########################


modB <- glm(1/time ~ wl + sl + wn + sn, family=Gamma(link="log"))
summary(modB)



######################### GAMMA EXAMPLE #######################################
n = 1e4

### Kappa
k0 <- log(2.00)
k1 <- log(0.99)
k2 <- log(0.97)

wn   <- rbinom(n,  20, 0.2)
sn   <- rbinom(n, 100, 0.7)
epsk <- rnorm (n,   0, 1e-2)

kappa = k0 + k1*wn + k2*sn + epsk

eKap = exp(kappa)

summary(eKap)


###theta 
t0 <- log(3.00)
t1 <- log(0.95)
t2 <- log(0.93)

wl   <- rbinom(n, 10, 0.5)
sl   <- rbinom(n, 10, 0.1)
epst <- rnorm (n,  0, 1e-2)

theta = t0 + t1*wl + t2*sl + epst

eThe = exp(theta)

summary(eThe)




### Independent variable 

depVar = rgamma(n, shape=eKap, scale=eThe)

time = 1/depVar



#dt <- data.frame( witches=wn, soldiers=sn, witchlevel=wl, soldierlevel=sl, time=time, depVar=depVar)

#write.csv(dt, "Witch data.csv", row.names=FALSE)
#getwd()





######################### VGLM #############################################################

Hlist <- list(
     "(Intercept)" = matrix( c(0,1,1,0), ncol=2),
     "wl" = rbind(1, 0),
     "sl" = rbind(1, 0),
     "wn" = rbind(0, 1),
     "sn" = rbind(0, 1)
     )

mod22 <- vglm(1/time ~ wl + sl + wn + sn, family = gammaR(zero=NULL), constraints = Hlist) ## BEST MODEL

summary(mod22)

coef(mod22, matrix=TRUE)

# Real values
# c1 <- +0.69315	c2 <- +1.09861
# wn <- -0.01005	wl <- -0.05129
# sn <- -0.03046	sl <- -0.07257
# KAPPA             THETA = 1/RATE  --> log(THETA) = -log(RATE)

mod23 <- vglm(1/time ~ wl + sl + wn + sn, family = gamma2(zero=NULL), constraints = Hlist)

summary(mod23)

coef(mod23, matrix=TRUE)


#####

