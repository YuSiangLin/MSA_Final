remove(list=ls())
getwd()
setwd("/Users/cychou/Documents/Work/Teaching/Exploratory Multivariate Data Analysis/Lecture Slides/R_code")
## Multiple regression

x<-c(150,150,150,200,200,200,250,250,250,300,300,300);
y<-c(77.4,76.7,78.2,84.1,84.5,83.7,88.9,89.2,89.7,94.8,94.7,95.9);
test<-data.frame(x,y);

mod1<-lm(y~x, data=test);
summary(mod1);
anova(mod1);



 ## with data from Greene (1993):
test<-read.csv("exAM.csv",header=T,sep=",");
fit<-lm(y~x, data=test)
summary(fit)
anova(fit)
plot(test$x,test$y)
#install.packages("alr3")
library(alr3)
pureErrorAnova(fit)

## Another way to show lack-of-fit
fit2<-lm(y~factor(x), data=test)
anova(fit,fit2)

## multi_regression.pptx
library(DAAG)
plot(weight ~ volume, data=allbacks, pch=c(16,1)[unclass(cover)])
# unclass(cover) gives the integer codes that identify levels
with(allbacks, text(weight ~ volume, labels=paste(1:15),
                    pos=c(2,4)[unclass(cover)]))

allbacks.lm <- lm(weight ~ volume+area, data=allbacks)
summary(allbacks.lm)
# Plot diagnostic plots
par(mfrow=c(2,2))    # Get all 4 plots on one page
plot(allbacks.lm)

#fit2<-lm(y[-11,])


#----------------------Testing below ---------------
# https://www.statmethods.net/stats/regression.html　－－－－－
attach(mtcars)

mydata<-mtcars;
y<-mtcars$mpg;
x1<-mtcars$wt; 
x2<-mtcars$disp;
x3<-mtcars$hp;
x4<-mtcars$drat;

# Multiple Linear Regression Example 
fit <- lm(y ~ x1 + x2 + x3, data=mydata)
summary(fit) # show results

# Other useful functions 
coefficients(fit) # model coefficients
confint(fit, level=0.95) # CIs for model parameters 
fitted(fit) # predicted values
residuals(fit) # residuals
anova(fit) # anova table 
vcov(fit) # covariance matrix for model parameters 
influence(fit) # regression diagnostics


# diagnostic plots 
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(fit)

# compare models
fit1 <- lm(y ~ x1 + x2 + x3 + x4, data=mydata)
fit2 <- lm(y ~ x1 + x2)
anova(fit1, fit2)

# K-fold cross-validation
#install.packages("DAAG")
library(DAAG)
cv.lm(data=mydata, fit, m=3) # 3 fold cross-validation

# Assessing R2 shrinkage using 10-Fold Cross-Validation 

fit <- lm(y~x1+x2+x3,data=mydata) 

install.packages("bootstrap")
library(bootstrap)
# define functions 
theta.fit <- function(x,y){lsfit(x,y)}
theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 

names(mydata)[names(mydata)=="mpg"]<-"y"
names(mydata)[names(mydata)=="wt"]<-"x1"
names(mydata)[names(mydata)=="disp"]<-"x2"
names(mydata)[names(mydata)=="hp"]<-"x3"
names(mydata)[names(mydata)=="drat"]<-"x4"
           

# matrix of predictors
X <- as.matrix(mydata[c("x1","x2","x3")])
# vector of predicted values
y <- as.matrix(mydata[c("y")]) 

results <- crossval(X,y,theta.fit,theta.predict,ngroup=10)
cor(y, fit$fitted.values)**2 # raw R2 
cor(y,results$cv.fit)**2 # cross-validated R2

# -----------------------End testing

# Stepwise Regression
library(MASS)
fit <- lm(y~x1+x2+x3,data=mydata)
step <- stepAIC(fit, direction="both")
step$anova # display results

# All Subsets Regression
#install.packages("leaps")
library(leaps)
attach(mydata)
leaps<-regsubsets(y~x1+x2+x3+x4,data=mydata,nbest=10) 
# view results 
summary(leaps)
# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps,scale="r2")
# plot statistic by subset size 
library(car)
subsets(leaps, statistic="rsq")

# Calculate Relative Importance for Each Predictor
install.packages("relaimpo")
library(relaimpo)
calc.relimp(fit,type=c("lmg","last","first","pratt"),
            rela=TRUE)

# Bootstrap Measures of Relative Importance (1000 samples) 
boot <- boot.relimp(fit, b = 1000, type = c("lmg", 
                                            "last", "first", "pratt"), rank = TRUE, 
                    diff = TRUE, rela = TRUE)
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

