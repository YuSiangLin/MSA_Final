# install.packages("sda")
# library(sda)

# data("singh2002");
# head(singh2002)

# library(glmnet)
# fit <- glmnet(singh2002$x, singh2002$y, family = "binomial", pmax=20, alpha=1)
# fit <- glmnet(as.matrix(x), y, family = "binomial", pmax=20, alpha=1)

#install.packages("ElemStatLearn")
#library(ElemStatLearn)

require(readr)
require(broom); 
require(dplyr)
require(glmnet) # lars is where the LASSO functions live
prostate <- read_delim("http://www-stat.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data",
                      "\t", escape_double = FALSE, trim_ws = TRUE)


# load the data set "prostate"
data(prostate)
attach(prostate)
my_data<- prostate[,-c(1,11)];
cor_my<-cor(my_data);

my_data$svi <- factor(my_data$svi)
my_data$gleason <- factor(my_data$gleason)

pairs(lpsa ~ lcavol + lweight + age + lbph + lcp + pgg45, data = my_data)
cor(my_data[,c(1,2,3,4,6,8,9)])

boxplot(lpsa ~ svi, data = my_data)
fit <- lm(lpsa ~ ., data = my_data)
summary(fit)

my_data$lpsa <- log(my_data$lpsa)
my_data$psa <- NULL
pairs(lpsa ~ lcavol + lweight + age + lbph + lcp + pgg45, data = my_data)

fit <- lm(lpsa ~ ., data = my_data)
summary(fit)

# leaps() performs an exhaustive search for the best subsets of the variables in x
# for predicting y in linear regression, using an efficient branch-and-bound algorithm. 
# It is a compatibility wrapper for regsubsets does the same thing better.
library(leaps)
fit <- regsubsets(lpsa ~ ., data = my_data, intercept = TRUE, method = "exhaustive")
summary(fit)

summary(fit)$adjr2
fit <- lm(lpsa ~ lcavol + lweight + svi, data = my_data)
plot(fit)




#----------

data("mtcars")
my_data<-mtcars;
head(mtcars);
res<-cor(my_data);
round(res,2);

ols<-lm(mpg ~ cyl + disp + hp + wt, data=my_data)
anova(ols)


#(α = 1 LASSO, α = 0 Ridge Regularization)
#install.packages("glmnet")
library(glmnet)

# Glmnet returns a sequence of different models for different values of λ.
mod<-glmnet(as.matrix(mtcars[,-1]), mtcars[,1], standardize=TRUE, alpha=1)

#cv.glmnet: Cross-validation for glmnet, default 10-fold
#Does k-fold cross-validation for glmnet, produces a plot, and returns a value for lambda
cvfit<-cv.glmnet(as.matrix(mtcars[,-1]),mtcars[,1])
glmcoef<-coef(mod,cvfit$lambda.min)
coef.increase<-dimnames(glmcoef[glmcoef[,1]>0,0])[[1]]
coef.decrease<-dimnames(glmcoef[glmcoef[,1]<0,0])[[1]]

#get ordered list of variables as they appear at smallest lambda
allnames<-names(coef(mod)[,
                          ncol(coef(mod))][order(coef(mod)[,
                          ncol(coef(mod))],decreasing=TRUE)])

#remove intercept
allnames<-setdiff(allnames,allnames[grep("Intercept",allnames)])

#assign colors
cols<-rep("gray",length(allnames))
cols[allnames %in% coef.increase]<-"green"      # higher mpg is good
cols[allnames %in% coef.decrease]<-"red"        # lower mpg is not

#Plot a model's response over a range of predictor values (the model surface)
install.packages("plotmo") 
library(plotmo)
plot_glmnet(mod,label=TRUE,s=cvfit$lambda.min,col=cols)

#if you don't believe hp or am are non-zero look at glmcoef
glmcoef

plot(lasso,label=TRUE)
cv<-cv.glmnet(M[, c(1,3,5)], mtcars[, 1], standardize=TRUE, ,type.measure='mse', nfolds=5,alpha=1)
