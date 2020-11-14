# ANOVA
setwd("/Users/cychou/Documents/Work/Teaching/Exploratory Multivariate Data Analysis/Lecture Slides/R_code");
methods<-read.csv("methods.csv",header=T,sep=",");
methods$method<-as.factor(methods$method)
mod1<-lm(X1~method,data=methods)
anova(mod1)


# MANOVA
install.packages("dplyr")
library(dplyr)

combined<-cbind(methods$X1,methods$X2)
mod2<-manova(combined~as.factor(method), data=methods)
summary(mod2, test="Pillai")
summary(mod2, test="Wilks")
summary(mod2, test="Roy")
summary(mod2, test="Hotelling-Lawley")


###  Eygptia skulls 
install.packages("HSAUR")
library(HSAUR) # load the library, must be installed before
data("skulls", package = "HSAUR") # load into the workspace
skulls # see the content of the data.frame
help(skulls)
summary(skulls)
#write.csv(skulls, file = "skulls.csv",row.names=FALSE)
Y<-cbind(skulls$mb, skulls$bh, skulls$bl, skulls$nh);
X<-as.factor(skulls$epoch);
fit<-manova(Y~X, data=skulls)
summary(fit, test="Pillai")
summary(fit, test="Wilks")
summary(fit, test="Roy")
summary(fit, test="Hotelling-Lawley")
T=matrix(c(3563.89,-222.81,-615.16,426.73,
           -222.81,3635.16,1046.28,346.47,
           -615.16,1046.28,4309.27,-16.40,
           426.73,346.47,-16.40,1533.33),nrow=4,ncol=4,byrow=T)

W=matrix(c(3061.07,5.33,11.47,291.30,
           5.33,3405.27,754.00,412.53,
           11.47,754.00,3505.97,164.33,
           291.30,412.53,164.33,1472.13),nrow=4,ncol=4,byrow=T)
B=T-W;
eigen(solve(W)%*%B)

### Rootstock.dat
root<-read.table("rootstock.dat", header=FALSE);
root<-data.frame(root)
Y<-cbind(root$V2, root$V3, root$V4, root$V5);Y
X<-as.factor(root$V1);
fit_root<-manova(Y~X, data=root)
summary(fit_root, test="Pillai")
summary(fit_root, test="Wilks")
summary(fit_root, test="Roy")
summary(fit_root, test="Hotelling-Lawley")
###
beetles.DAT
mod3<-manova(cbind(skulls$mb,skulls$bh,skulls$bl,skulls$nh)~epoch,data=skulls)
summary(mod3)

## Homogeneity of covariances test
install.packages("covTestR")
library(covTestR)

homogeneityCovariances(x, ..., covTest = BoxesM)
