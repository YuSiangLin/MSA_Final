## Load Bumpus sparrow Data

# lapply(libraries, function(x) if (!(x %in% installed.packages())) {
#   install.packages(x)
# })
# lapply(libraries, library, quietly = TRUE, character.only = TRUE)
# 


## Load Bumpus sparrow Data
setwd("/Users/cychou/Documents/Work/Teaching/Exploratory Multivariate Data Analysis/Lecture Slides/R_code")

bird<-read.table("bumpus.dat", header=TRUE); head(bird)
# length, alar, lbh, lhum, lkeel;  # col 2, 3, 5, 6, 10

X1_5<-c(2:3,5:6,10);
bird_cor<-cor(bird[,X1_5])
bird_cov<-cov(bird[,X1_5])

eigen(bird_cor);
bird_pca1<-prcomp(bird[,X1_5],scale=TRUE); summary(bird_pca1);
bird_pca2<-princomp(bird[,X1_5],cor=TRUE); summary(bird_pca2, loadings=TRUE);
plot(bird_pca2)
biplot(bird_pca2,col=c("red","green"))

zdat<-scale(as.matrix(bird[,X1_5]));
pca.score<- zdat %*% (bird_pca2$loadings);
cor(as.matrix(bird)[,1:4],pca.score[,1])

#### Test scores of 10 students in 4 subjects
Ch<-c(85,90,60,70,68,77,50,80,85,55);
En<-c(76,95,45,65,56,80,30,70,75,60);
Math<-c(60,80,38,60,70,65,40,60,65,40);
Social<-c(85,72,80,76,70,68,80,66,84,50);
student<-data.frame(Ch,En,Math,Social)
cor(student)
eigen(cor(student))
stu_pca2<-princomp(student,cor=TRUE); summary(stu_pca2,loadings=TRUE);
plot(stu_pca2, type="lines")

zdat<-scale(as.matrix(student));
pca.score<- zdat %*% (stu_pca2$loadings);pca.score
cor(as.matrix(student)[,1:4],pca.score[,1])

## R commands for STAT 494 (39241, 39243)
## Special Topics in Statistics, Probability and Operations Research:
## Statistical Techniques for Machine Learning and Data Mining
## Fall Semester 2016
## Version: 2016.09.04
## Reference: http://homepages.math.uic.edu/~jyang06/stat494/stat494.html

## R package: ElemStatLearn, version 2015.6.26
## Data Sets, Functions and Examples from the Book (ESL): 
## "The Elements of Statistical Learning, Data Mining, Inference, and Prediction" 
## by Trevor Hastie, Robert Tibshirani and Jerome Friedman, Second Edition, 2009

## At Cran: https://cran.r-project.org/web/packages/ElemStatLearn/index.html
## Reference: Section 3.5 in the ESL book

## Principal component analysis using R
## Reference: https://www.cs.princeton.edu/picasso/mats/PCA-Tutorial-Intuition_jp.pdf
## A toy example
N <- 300         # number of data points
set.seed(323)
z1 <- rnorm(N, mean=0, sd=3)
z2 <- rnorm(N, mean=0, sd=1)
x1 <- z1 - z2
x2 <- z1 + z2
plot(x1, x2)
# 1st principal component: (a1, a2) maximizing var(a1*x1 + a2*x2), subject a1^2 + a2^2 = 1
abline(0,1)  # that is, (a1, a2)=(1, 1)/sqrt(2)
# 2nd principal component: (b1, b2) maximizing var(b1*x1 + b2*x2), subject to b1^2 + b2^2 = 1 and cov(a1*x1+a2*x2, b1*x1+b2*x2)=0
abline(0,-1) # that is, (b1, b2)=(-1, 1)/sqrt(2)

# using R function "prcomp"
pca = prcomp(cbind(x1,x2))  # applying principal component analysis
plot(pca) 
pca
# Standard deviations:
#   [1] 4.413913 1.401510
# Rotation:
#   PC1        PC2
# x1 0.7018288 -0.7123457
# x2 0.7123457  0.7018288


## A real example
## Reference https://www.r-bloggers.com/principal-component-analysis-using-r/
data("crimtab") #load data: Student's 3000 Criminals Data

dim(crimtab)    # check dimensions, rows: finger lengths; columns: body heights
#[1] 42 22
sum(crimtab)    # 3000

plot(as.numeric(colnames(crimtab)), apply(crimtab,2,var),type="b", xlab="height", ylab="variance") #check the variance accross the variables

pca =prcomp(crimtab) #applying principal component analysis on crimtab data
#pca$sdev
plot(pca) # the top two principal components may be chosen


## load the package  "ElemStatLearn" 
library("ElemStatLearn")

# load the data set "prostate"
data(prostate)

# partition the original data into training and testing datasets
train <- subset( prostate, train==TRUE )[,1:9]
test  <- subset( prostate, train==FALSE)[,1:9]  
# Note: There is a typo in "prostate" help file in "ElemStatLearn" package
# In "test  <- subset( prostate, train=FALSE )[,1:9]", "=" should be "=="

# standardization of predictors
trainst <- train
for(i in 1:8) {
  trainst[,i] <- trainst[,i] - mean(prostate[,i]);
  trainst[,i] <- trainst[,i]/sd(prostate[,i]);
}
testst <- test
for(i in 1:8) {
  testst[,i] <- testst[,i] - mean(prostate[,i]);
  testst[,i] <- testst[,i]/sd(prostate[,i]);
}

#### Principal Components Regression
library(pls)
set.seed(2)
pcr.fit=pcr(lpsa~., data=trainst, scale=F, validation="CV", segments=10)
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
# find the best number of components, regenerating part of Figure 3.7 on page 62
itemp=which.min(pcr.fit$validation$PRESS)     # 8
itemp.mean=pcr.fit$validation$PRESS[itemp]/67 # 0.5925903
mean((pcr.fit$validation$pred[,,itemp]-trainst[,9])^2) # 0.5925903
itemp.sd=sd((pcr.fit$validation$pred[,,itemp]-trainst[,9])^2)/sqrt(67)   # 0.1096081
abline(h=itemp.mean+itemp.sd, lty=2)
k.pcr = min((1:pcr.fit$validation$ncomp)[pcr.fit$validation$PRESS/67 < itemp.mean+itemp.sd])  # the chosen k = 5
abline(v=k.pcr, lty=2)   
pcr.fit$coefficients[,,k.pcr]   # fitted model with chosen number of components
#      lcavol      lweight          age         lbph          svi          lcp      gleason        pgg45 
# 0.337864476  0.255072247 -0.058466081  0.265979630  0.246430183  0.251430285 -0.009360589  0.069198805 

# estimating mean prediction error
test.pcr=predict(pcr.fit,as.matrix(testst[,1:8]),ncomp=k.pcr)
# mean (absolute) prediction error
mean(abs(test[,9]-test.pcr))                # 0.5764996
# mean (squared) prediction error
mean((test[,9]-test.pcr)^2)                 # 0.5225736
# standard error of mean (squared) prediction error
sd((test[,9]-test.pcr)^2)/sqrt(30)          # 0.1245866


#### Partial Least Squares using R
#### Section 3.5.2 in ESL book, or Section 6.3.2 in ISL book (An Introduction to Statistical Learning: with Applications in R)
library(pls)
set.seed(1)
plsr.fit=plsr(lpsa~., data=trainst, scale=F, validation="CV", segments=10)
summary(plsr.fit)
validationplot(plsr.fit,val.type="MSEP")
# find the best number of components, regenerating part of Figure 3.7 on page 62
itemp=which.min(plsr.fit$validation$PRESS)     # 6
itemp.mean=plsr.fit$validation$PRESS[itemp]/67 # 0.5683932
mean((plsr.fit$validation$pred[,,itemp]-trainst[,9])^2) # 0.5683932
itemp.sd=sd((plsr.fit$validation$pred[,,itemp]-trainst[,9])^2)/sqrt(67)   # 0.1049675
abline(h=itemp.mean+itemp.sd, lty=2)
k.plsr = min((1:plsr.fit$validation$ncomp)[plsr.fit$validation$PRESS/67 < itemp.mean+itemp.sd])  # the chosen k = 2
abline(v=k.plsr, lty=2)   
plsr.fit$coefficients[,,k.plsr]   # fitted model with chosen number of components
#     lcavol     lweight         age        lbph         svi         lcp     gleason       pgg45 
# 0.41925335  0.34486789 -0.02588107  0.21992197  0.24319848  0.07845299  0.01083593  0.08372182 

# estimating mean prediction error
test.plsr=predict(plsr.fit,as.matrix(testst[,1:8]),ncomp=k.plsr)
# mean (absolute) prediction error
mean(abs(test[,9]-test.plsr))                # 0.5548436
# mean (squared) prediction error
mean((test[,9]-test.plsr)^2)                 # 0.526937
# standard error of mean (squared) prediction error
sd((test[,9]-test.plsr)^2)/sqrt(30)          # 0.15038036
