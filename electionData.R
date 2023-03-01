
library(VGAM) 
source("http://rfs.kvasaheim.com/rfs.R")

############################ ELECTION EXAMPLE ########################
###### OLS

library(readr)
dk <- read_csv("C:/Users/18153/Downloads/kingdom_data - Sheet1.csv")
View(dk)
attach(dk)

depvar <- cbind(Invalid, Valid)
dMyself <- Myself/Valid
dDarrow <- King_Darrow/Valid
pInv <-Invalid/(Valid+Invalid)

modA <- lm(pInv ~ PDarrow)
summary(modA)
eA <- residuals(modA)

#### Normality
qqnorm(eA, pch=19)
qqline(eA)
hist(eA)
overlay(eA)
shapiroTest(eA) ##not normal

### Expected value
par(mar=c(3,3,0,0)+1, las=1, family="serif")
par(cex.lab=1.2, font.lab=2)
par(xaxs="i",yaxs="i")
plot.new()
plot.window( xlim=c(0.93,1), ylim=c(-0.3,0.3))
plot(PDarrow, eA, xlim=c(0.93,1), ylim=c(-0.3,0.3), yaxt="true", xlab="Proportion of King Darrow's Votes",
     ylab="Residuals")
abline(h = 0, lty=2)


runs.test(eA, order=PDarrow) ##  0

####variacne
x = seq(0,1,by=0.037)
y = 2.1112 - 2.1013*x + eA
plot(PDarrow,eA, pch=21, bg="dodgerblue", xlab="Proportion of King Darrow's Votes", ylab="Residuals", yaxt="true", 
     xlim=c(0.94,0.99), ylim=c(-0.1,0.1))
abline(h=0,lty=2)
bptest(modA) ##var is constant

##indepdendence
summary(modA)








########## NEW MODEL.. ISSUES WITH LOGIT ON DEPVAR ###########

modB <- lm( logit(pInv) ~ PDarrow)
eB <- residuals(modB)

#### Normality
qqnorm(eB)
qqline(eB)
hist(eB)
overlay(eB)
shapiroTest(eB) ## normal

### Expected value
plot(PDarrow,eB) ##resids ev 0

runs.test(eB, order=PDarrow) ## 0

####variacne
bptest(modB) ##var is constant

summary(modB)

par(mar=c(3,3,0,0)+1, las=1, family="serif")
par(cex.lab=1.2, font.lab=2)
par(xaxs="i",yaxs="i")
plot.new()
plot.window( xlim=c(0.93,1), ylim=c(0,0.25))
plot(PDarrow, pInv, xlim=c(0.93,1), ylim=c(0,0.25), yaxt="true", xlab=c("Valid Votes for Sisi"),
     ylab=c("Proportion of Invalid Votes"))
newX = seq(0, 1, by=.01)
y1 = logit.inv(predict(modB, newdata=data.frame(PDarrow=newX)) )
y2 <- predict(modA, newdata=data.frame(PDarrow=newX))
lines(newX,y1,col="green")
lines(newX, y2, col="purple")
lines(newX, y3, col="orange")
lines(newX, y4[1:101], col="Black")
points(PDarrow, pInv, col="darkgray", pch=20)
legend(0.98, 0.20, legend=c("OLS", "Transformed OLS","GLM", "VGLM"),
       col=c("Purple", "Green", "orange", "dark red"), lty=1, cex=0.8)











################### GLM

modC <- glm(depvar ~ PDarrow, family=binomial(link="logit"))
summary(modC)

par(mar=c(3,3,0,0)+1, las=1, family="serif")
par(cex.lab=1.2, font.lab=2)
par(xaxs="i",yaxs="i")
plot.new()
plot.window( xlim=c(0.92,1), ylim=c(0,0.4))
plot(PDarrow, pInv, xlim=c(0.92,1), ylim=c(0,0.4), yaxt="true", xlab=c("Proportion of King Darrow's Votes"),
     ylab=c("Proportion of Invalid Votes"))
newX = seq(0, 1, by=.01)
y3 = logit.inv(predict(modC, newdata=data.frame(PDarrow=newX)) )
lines(newX,y3,col="orange")
points(PDarrow, pInv, col="steelblue", pch=19)









###################3 VGLM

modD <- vglm(depvar ~ PDarrow, family=betabinomial)
summary(modD)

par(mar=c(3,3,0,0)+1, las=1, family="serif")
par(cex.lab=1.2, font.lab=2)
par(xaxs="i",yaxs="i")
plot.new()
plot.window( xlim=c(0.92,1), ylim=c(0,0.4))
plot(PDarrow, pInv, xlim=c(0.92,1), ylim=c(0,0.4), yaxt="true", xlab=c("Proportion of Valid Votes for King Darrow"),
     ylab=c("Proportion of Invalid Votes"))
newX = seq(0, 1, by=.01)
y4 = logit.inv(predict(modD, newdata=data.frame(PDarrow=newX)) )
lines(newX,y4[1:101],col="dark red")
points(PDarrow, pInv, col="steelblue", pch=19)
