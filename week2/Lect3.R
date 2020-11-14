#### Exploratory Multivariate Data Analysis ####
###  Lecture 3 for comparison of means and outlier detection ####

remove(list=ls())
getwd()
setwd("/Users/cychou/Documents/Work/Teaching/Exploratory Multivariate Data Analysis/Lecture Slides/R_code")


##### ------Two sample multivariate T2 test --- Male vs. Female body shapes ---#####
n1<-10; n2<-10; p<-3;
chest<-c(34,37,38,36,38,43,40,38,40,41,36,36,34,33,36,37,34,36,38,35);
waist<-c(30,32,30,33,29,32,33,30,30,32,24,25,24,22,26,26,25,26,28,23);
hips<-c(32,37,36,39,33,38,42,40,37,39,35,37,37,34,38,37,38,37,40,35);
gender<-rep(c("male","female"),each=10)
measure<-data.frame(chest,waist,hips,gender)

cov_all<-cov(measure[, c("chest", "waist", "hips")]); cov_all;

women<-subset(measure, gender == "female")[,  c("chest", "waist", "hips")];
men<-subset(measure, gender == "male")[,  c("chest", "waist", "hips")]; 
covF<-cov(women);
covM<-cov(men);
#where the subset() returns all observations corresponding to females (first statement) 
# or males (second statement).

# pooled covariance matrix for T2 calculation
cov_pooled<- (covF*(n1-1)+covM*(n2-1))/(n1+n2-2);
invCp<-solve(cov_pooled); invCp;
invC<-solve(cov_all); invC; # This will be the F value of 5.04, which is not correct. 

meanF<-colMeans(subset(measure, gender == "female")[,1:3]);
meanM<-colMeans(subset(measure, gender == "male")[,1:3]);       

T2<-(n1*n2)/(n1+n2)*(meanF-meanM)%*%invCp%*%t(t(meanF-meanM));T2
F_calc<-(n1+n2-p-1)*T2/(n1+n2-2)/p; F_calc
F_crit<-qf(0.95,p,n1+n2-p-1); F_crit


##---------- Applying Hotelling package ----------##
#install.packages("Hotelling")
library (Hotelling)
mod<-hotelling.test(men,women); mod


######  Paired two-sample multivariate T2 test--- before/after treatment  ##########
n=6; p=2; 
X11=c(12.5,14.2, 10.8,13.4, 11.9,12.0);  # uraic acid before
X12<-c(6.5,7.8,6.0,7.2,6.8,5.9);         # uraic acid after
X21<-c(220,260,180,200,280,170);         # cholesterol before
X22<-c(190,250,190,220,240,180);         # cholesterol after
D1<-X12-X11;  D1
D2<-X22-X21;  D2

data<-data.frame(D1,D2);
cov_D<-cov(data[,c("D1","D2")]); cov_D
inv_cov_D <- solve(cov_D); inv_cov_D
D_mean=c(mean(D1),mean(D2)); D_mean

T2<- n*(D_mean)%*%inv_cov_D%*%t(t(D_mean)); T2
F_calc<-(n-p)/p*T2/(n-1); F_calc
F_crit<-qf(0.95,p,n-p); F_crit


########-----Two sample multivariate T2 test----Teacher's evaluation---################
install.packages("ICSNP")
library("ICSNP")
m1 <- with(measure, HotellingsT2(cbind(chest,waist,hips) ~ gender))
m1

# Multivariate 2 sample T2 test
#install.packages("ICSNP")
library("ICSNP")
math.teach <- data.frame(teacher = factor(rep(1:2,c(3, 6))), 
                         satis = c(1, 3, 2, 4, 6, 6, 5,5, 4), 
                         know = c(3, 7, 2, 6, 8, 8, 10, 10, 6))

with(math.teach, tapply(know, teacher, mean))
with(math.teach, tapply(satis, teacher, mean))

par(mfrow = c(2, 1))
boxplot(know ~ teacher, math.teach, main = "Teacher Knowledge", horizontal = T)
boxplot(satis ~ teacher, math.teach, main = "Teacher Satisfaction",horizontal = T)

m1 <- with(math.teach, HotellingsT2(cbind(satis, know) ~ teacher)); m1

# Although this is labeled “T.2”, this is actually an F transformation of the computed T2, 
# so no need to run your own transformation.



### Detetion of outliers #####

### Detetion of univariate outliers #####
HBAT<-read.csv("HBAT.csv", header=T, sep=",")
x7<-scale(HBAT$X7);
x8<-scale(HBAT$X8);
x19<-scale(HBAT$X19);
abs(x7>2.5);
#abs(x8>=2.5);

### Detetion of bivariate outliers #####
#install.packages("car")
library(car)
dataEllipse(HBAT$X19,HBAT$X6, levels=c(0.975))
dataEllipse(HBAT$X19,HBAT$X7, levels=c(0.95))

### Detetion of multivariate outliers #####
X<-HBAT[,7:19];  # Read X6 to X18 to X matrix
p<-ncol(X); 
Sx <- cov(X)
D2 <- mahalanobis(X, colMeans(X), Sx);D2


par(mfrow=c(1,1))
chisplot(as.matrix(X))
D2/(p); # >2.5 are multivariate outliers 


####  Multivariate normality check ####

"huswif" <- 
  structure(.Data = list(c(49, 25, 40, 52, 58, 32, 43, 47, 31, 26), 
                         c(1809, 1841, 1659, 1779, 1616, 1695, 1730, 1740, 1685, 1735),
                         c(43, 28, 30, 57, 52, 27, 52, 43, 23, 25),
                         c(1590, 1560, 1620, 1540, 1420, 1660, 1610, 1580, 1610, 1590),
                         c(25, 19, 38, 26, 30, 23, 33, 26, 26, 23)
  )
  , names = c("Hage", "Hheight", "Wage", "Wheight", "Hagefm")
  , row.names = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
  , class = "data.frame"
  )

write.csv(huswif, file = "huswif.csv",row.names=FALSE)
# write.csv(USairpollution, file = "USairpollution.csv",row.names=FALSE)


#### Plotting Euclidean Distances

dis<-dist(huswif)
dis

#### Plotting for Multivariate Normality
par(mfrow=c(1,5))
qqnorm(huswif$Hage, ylab="Ordered Observations Hage")
qqline(huswif[,1])
qqnorm(huswif$Hheight, ylab="Ordered Observations Hheight")
qqline(huswif[,2])
qqnorm(huswif$Wage, ylab="Ordered Observations Wage")
qqline(huswif[,3])
qqnorm(huswif$Wheight, ylab="Ordered Observations Wheight")
qqline(huswif[,4])
qqnorm(huswif$Hagefm, ylab="Ordered Observations Hagefm")
qqline(huswif[,5])

#### Chi-Square plot for 
chisplot <- function(x) {
  if (!is.matrix(x)) stop("x is not a matrix")
  
  ### determine dimensions
  n <- nrow(x)
  p <- ncol(x)
  #
  xbar <- apply(x, 2, mean)
  S <- var(x)
  S <- solve(S)
  index <- (1:n-0.5)/n
  #
  xcent <- t(t(x) - xbar)
  di <- apply(xcent, 1, function(x,S) x %*% S %*% x,S)
  #
  quant <- qchisq(index,p)
  plot(quant, sort(di), ylab = "Ordered distances",
       xlab = "Chi-square quantile", lwd=2,pch=1)
  
}
par(mfrow=c(1,1))
chisplot(as.matrix(df1))

