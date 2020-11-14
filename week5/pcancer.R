require(readr)
require(broom); 
require(dplyr)
require(glmnet) # lars is where the LASSO functions live
pcancer <- read_delim("http://www-stat.stanford.edu/~hastie/ElemStatLearn/datasets/prostate.data",
                      "\t", escape_double = FALSE, trim_ws = TRUE)
head(pcancer)

pr.train <- pcancer[which(pcancer$train),2:10]
pr.test <- pcancer[-which(pcancer$train),2:10]
pairs(pr.train[,1:9], pch=19, cex=.25)
round(cor(pr.train[,1:9]),3)

pr.ls <- lm(lpsa ~ ., data=pr.train)
summary(pr.ls)$coef
#-----
set.seed(47)
fewpts = sample(c(TRUE, FALSE), 67, replace=TRUE, prob=c(.1,.9)) # sample 10% of the training data
pr.ls.sub <- lm(lpsa~., data=pr.train, subset=fewpts)
summary(pr.ls.sub)

x.fewpts <- as.matrix(cbind(int = rep(1, dim(pr.train[fewpts,])[1]), pr.train[fewpts,-9]))
dim(x.fewpts)

solve(t(x.fewpts) %*% x.fewpts)

## Error in solve.default(t(x.fewpts) %*% x.fewpts): Lapack routine dgesv: system is exactly singular: U[6,6] = 0
x.full <- as.matrix(cbind(int = rep(1, dim(pr.train)[1]),pr.train[,-9]))
solve(t(x.full) %*% x.full) # note that it is invertible w/all the data

###---- AIC model building ---------#####
pr.fwd <- step(lm(lpsa ~ 1, data=pr.train),
               ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45,
               data=pr.train, direction="forward")
pr.bck <- step(pr.ls, direction="backward")

tidy(pr.fwd)
tidy(pr.bck)

#### Comparing Models
forw.pred <- predict(pr.fwd, newdata = pr.test)
back.pred <- predict(pr.bck, newdata = pr.test)

sum((forw.pred - pr.test$lpsa)^2)
## [1] 14
sum((back.pred - pr.test$lpsa)^2)
## [1] 15

### ------  Ridge Regression ----------###
lambda.grid =10^seq(5,-2, length =100)
# note the form of the regression function is glmnet(x,y,...)
# alpha = 0 gives RR, alpha=1 gives Lasso
pr.ridge <- glmnet(as.matrix(pr.train[,1:8]), pr.train$lpsa, alpha=0,
                   lambda = lambda.grid, standardize=TRUE)
dim(coef(pr.ridge)) # 9 coef, 100 lambda values
coef(pr.ridge)[,2] # note glmnet unstandardizes the variables

lambda.grid[90]
coef(pr.ridge)[,90] # note glmnet unstandardizes the variables

pr.ridge.cv <- cv.glmnet(as.matrix(pr.train[,1:8]), pr.train$lpsa,
                         alpha=0, lambda = lambda.grid, standardize=TRUE)
plot(pr.ridge.cv)
abline(v=log(pr.ridge.cv$lambda.min), col="green")

colors <- rainbow(8)
plot(pr.ridge, xvar="lambda", xlim=c(-6,10),col=colors)
pr.ridge.cv$lambda.min  #value of lambda that gives minimum cvm.

## [1] 0.11
abline(v=log(pr.ridge.cv$lambda.min))
abline(h=0, lty=2)
text(rep(-6.5, 9), coef(pr.ridge)[-1,length(lambda.grid)], colnames(pr.train)[-9], pos=4, col=colors)

plot(pr.ridge, xvar="dev",col=colors, label=TRUE)
abline(h=0, lty=2)

plot(pr.ridge, xvar="norm",col=colors, label=TRUE)
abline(h=0, lty=2)

coef(pr.ridge.cv, s = "lambda.min")

###  ----- Lasso model building  --------####
# note the form of the regression function is glmnet(x,y,...)
# alpha = 0 gives RR, alpha=1 gives Lasso
pr.lasso <- glmnet(as.matrix(pr.train[,1:8]), pr.train$lpsa, alpha=1,
                   lambda = lambda.grid, standardize=TRUE)

# Choosing lambda 

dim(coef(pr.lasso)) # 9 coef, 100 lambda values
## [1] 9 100
lambda.grid[2]
## [1] 84975
coef(pr.lasso)[,2] # note glmnet unstandardizes the variables
## (Intercept) lcavol lweight age lbph svi
## 2.5 0.0 0.0 0.0 0.0 0.0
## lcp gleason pgg45
## 0.0 0.0 0.0
lambda.grid[90]
## [1] 0.051
coef(pr.lasso)[,90] # note glmnet unstandardizes the variables
pr.lasso.cv <- cv.glmnet(as.matrix(pr.train[,1:8]), pr.train$lpsa,
                         alpha=1, lambda = lambda.grid, standardize=TRUE)
plot(pr.lasso.cv)
abline(v=log(pr.lasso.cv$lambda.min), col="green") # v the x-value(s) for vertical line(s).

plot(pr.lasso, xvar="lambda", xlim=c(-6,10), col=colors)
pr.lasso.cv$lambda.min
## [1] 0.012
abline(v=log(pr.lasso.cv$lambda.min)) # v the x-value(s) for vertical line(s).
abline(h=0, lty=2) # h the y-value(s) for horizontal line(s).
text(rep(-6.5, 9), coef(pr.lasso)[-1,length(lambda.grid)], colnames(pr.train)[-9], pos=4, col=colors)

plot(pr.lasso, xvar="dev",col=colors, label=TRUE)
abline(h=0, lty=2)

coef(pr.lasso.cv, s = "lambda.min")
## 9 x 1 sparse Matrix of class "dgCMatrix"
## 1
## (Intercept) 0.1756
## lcavol 0.5475
## lweight 0.5985
## age -0.0155
## lbph 0.1360
## svi 0.6780
## lcp -0.1520
## gleason .
## pgg45 0.0076

#### Comparing Models
forw.pred <- predict(pr.fwd, newdata = pr.test)
back.pred <- predict(pr.bck, newdata = pr.test)
ridge.pred <- predict(pr.ridge.cv, newx = as.matrix(pr.test[,1:8]),
                      s = "lambda.min")
lasso.pred <- predict(pr.lasso.cv, newx = as.matrix(pr.test[,1:8]),
                      s = "lambda.min")
sum((forw.pred - pr.test$lpsa)^2)
## [1] 14
sum((back.pred - pr.test$lpsa)^2)
## [1] 15
sum((ridge.pred - pr.test$lpsa)^2)
## [1] 15
sum((lasso.pred - pr.test$lpsa)^2)
## [1] 15


