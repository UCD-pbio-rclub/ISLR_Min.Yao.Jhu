# chapter5-1
Min-Yao  
2018年2月4日  

# Chaper 5 Lab: Cross-Validation and the Bootstrap

# The Validation Set Approach


```r
library(ISLR)
set.seed(1)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
```

```
## [1] 26.14142
```

```r
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
```

```
## [1] 19.82259
```

```r
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
```

```
## [1] 19.78252
```

```r
set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
mean((mpg-predict(lm.fit,Auto))[-train]^2)
```

```
## [1] 23.29559
```

```r
lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg-predict(lm.fit2,Auto))[-train]^2)
```

```
## [1] 18.90124
```

```r
lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg-predict(lm.fit3,Auto))[-train]^2)
```

```
## [1] 19.2574
```


# Leave-One-Out Cross-Validation


```r
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
```

```
## (Intercept)  horsepower 
##  39.9358610  -0.1578447
```

```r
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)
```

```
## (Intercept)  horsepower 
##  39.9358610  -0.1578447
```

```r
library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta
```

```
## [1] 24.23151 24.23114
```

```r
cv.error=rep(0,5)
for (i in 1:5){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
 }
cv.error
```

```
## [1] 24.23151 19.24821 19.33498 19.42443 19.03321
```


# k-Fold Cross-Validation


```r
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
 }
cv.error.10
```

```
##  [1] 24.20520 19.18924 19.30662 19.33799 18.87911 19.02103 18.89609
##  [8] 19.71201 18.95140 19.50196
```


# The Bootstrap


```r
alpha.fn=function(data,index){
 X=data$X[index]
 Y=data$Y[index]
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 }
alpha.fn(Portfolio,1:100)
```

```
## [1] 0.5758321
```

```r
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
```

```
## [1] 0.5963833
```

```r
boot(Portfolio,alpha.fn,R=1000)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Portfolio, statistic = alpha.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##      original        bias    std. error
## t1* 0.5758321 -7.315422e-05  0.08861826
```


# Estimating the Accuracy of a Linear Regression Model


```r
boot.fn=function(data,index)
 return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
```

```
## (Intercept)  horsepower 
##  39.9358610  -0.1578447
```

```r
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
```

```
## (Intercept)  horsepower 
##  38.7387134  -0.1481952
```

```r
boot.fn(Auto,sample(392,392,replace=T))
```

```
## (Intercept)  horsepower 
##  40.0383086  -0.1596104
```

```r
boot(Auto,boot.fn,1000)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Auto, statistic = boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##       original      bias    std. error
## t1* 39.9358610  0.02972191 0.860007896
## t2* -0.1578447 -0.00030823 0.007404467
```

```r
summary(lm(mpg~horsepower,data=Auto))$coef
```

```
##               Estimate  Std. Error   t value      Pr(>|t|)
## (Intercept) 39.9358610 0.717498656  55.65984 1.220362e-187
## horsepower  -0.1578447 0.006445501 -24.48914  7.031989e-81
```

```r
boot.fn=function(data,index)
 coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = Auto, statistic = boot.fn, R = 1000)
## 
## 
## Bootstrap Statistics :
##         original        bias     std. error
## t1* 56.900099702  6.098115e-03 2.0944855842
## t2* -0.466189630 -1.777108e-04 0.0334123802
## t3*  0.001230536  1.324315e-06 0.0001208339
```

```r
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
```

```
##                     Estimate   Std. Error   t value      Pr(>|t|)
## (Intercept)     56.900099702 1.8004268063  31.60367 1.740911e-109
## horsepower      -0.466189630 0.0311246171 -14.97816  2.289429e-40
## I(horsepower^2)  0.001230536 0.0001220759  10.08009  2.196340e-21
```

# 3. We now review k-fold cross-validation.

## (a) Explain how k-fold cross-validation is implemented.

>  This approach involves randomly dividing the set of observations into k groups, or folds, of approximately equal size. The ﬁrst fold is treated as a validation set, and the method is ﬁt on the remaining k − 1 folds. The mean squared error, MSE 1 , is then computed on the observations in the held-out fold. This procedure is repeated k times; each time, a diﬀerent group of observations is treated as a validation set. This process results in k estimates of the test error, MSE 1 , MSE 2 , . . . , MSE k . The k-fold CV estimate is computed by averaging these values.

## (b) What are the advantages and disadvantages of k-fold cross-validation relative to:
## i. The validation set approach?
## ii. LOOCV?

> i.  Disadvantages: The validation estimate of the test error rate can be highly variable, depending on precisely which observations are included in the training set and which observations are included in the validation set.

> Since statistical methods tend to perform worse when trained on fewer observations, this suggests that the validation set error rate may tend to overestimate the test error rate for the model ﬁt on the entire data set.


> ii. LOOCV is a special case of k-fold CV in which k is set to equal n. The most obvious advantage is computational. LOOCV requires ﬁtting the statistical learning method n times. So, performing LOOCV may pose computational problems, especially if n is extremely large.

>  More important advantage of k-fold CV is that it often gives more accurate estimates of the test error rate than does LOOCV. This has to do with a bias-variance trade-oﬀ. LOOCV has higher variance than does k-fold CV with k < n.

# 5. In Chapter 4, we used logistic regression to predict the probability of default using income and balance on the Default data set. We will now estimate the test error of this logistic regression model using the validation set approach. Do not forget to set a random seed before beginning your analysis.

## (a) Fit a logistic regression model that uses income and balance to predict default.


```r
library(ISLR)

summary(Default)
```

```
##  default    student       balance           income     
##  No :9667   No :7056   Min.   :   0.0   Min.   :  772  
##  Yes: 333   Yes:2944   1st Qu.: 481.7   1st Qu.:21340  
##                        Median : 823.6   Median :34553  
##                        Mean   : 835.4   Mean   :33517  
##                        3rd Qu.:1166.3   3rd Qu.:43808  
##                        Max.   :2654.3   Max.   :73554
```

```r
glm.fits.de=glm(default~income+balance,data=Default,family=binomial)
summary(glm.fits.de)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = Default)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4725  -0.1444  -0.0574  -0.0211   3.7245  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.154e+01  4.348e-01 -26.545  < 2e-16 ***
## income       2.081e-05  4.985e-06   4.174 2.99e-05 ***
## balance      5.647e-03  2.274e-04  24.836  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2920.6  on 9999  degrees of freedom
## Residual deviance: 1579.0  on 9997  degrees of freedom
## AIC: 1585
## 
## Number of Fisher Scoring iterations: 8
```


## (b) Using the validation set approach, estimate the test error of this model. In order to do this, you must perform the following steps:

### i. Split the sample set into a training set and a validation set.


```r
summary(Default)
```

```
##  default    student       balance           income     
##  No :9667   No :7056   Min.   :   0.0   Min.   :  772  
##  Yes: 333   Yes:2944   1st Qu.: 481.7   1st Qu.:21340  
##                        Median : 823.6   Median :34553  
##                        Mean   : 835.4   Mean   :33517  
##                        3rd Qu.:1166.3   3rd Qu.:43808  
##                        Max.   :2654.3   Max.   :73554
```

```r
str(Default)
```

```
## 'data.frame':	10000 obs. of  4 variables:
##  $ default: Factor w/ 2 levels "No","Yes": 1 1 1 1 1 1 1 1 1 1 ...
##  $ student: Factor w/ 2 levels "No","Yes": 1 2 1 1 1 2 1 2 1 1 ...
##  $ balance: num  730 817 1074 529 786 ...
##  $ income : num  44362 12106 31767 35704 38463 ...
```

```r
set.seed(1)
train=sample(10000,5000)
default.train <- Default[train,]
dim(default.train)
```

```
## [1] 5000    4
```

```r
default.test <- Default[-train,]
dim(default.test)
```

```
## [1] 5000    4
```

```r
# nrow(Default)
# sample.split
```

### ii. Fit a multiple logistic regression model using only the training observations.


```r
attach(Default)
glm.fits.de1=glm(default~income+balance,data=Default,family=binomial,subset=train)
summary(glm.fits.de1)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = Default, subset = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.3583  -0.1268  -0.0475  -0.0165   3.8116  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.208e+01  6.658e-01 -18.148   <2e-16 ***
## income       1.858e-05  7.573e-06   2.454   0.0141 *  
## balance      6.053e-03  3.467e-04  17.457   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1457.0  on 4999  degrees of freedom
## Residual deviance:  734.4  on 4997  degrees of freedom
## AIC: 740.4
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.fits.de2=glm(default~poly(income,2)+poly(balance,2),data=Default,family=binomial,subset=train)
summary(glm.fits.de2)
```

```
## 
## Call:
## glm(formula = default ~ poly(income, 2) + poly(balance, 2), family = binomial, 
##     data = Default, subset = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.5077  -0.1296  -0.0529  -0.0217   3.7051  
## 
## Coefficients:
##                   Estimate Std. Error z value Pr(>|z|)    
## (Intercept)        -6.1036     0.5187 -11.768  < 2e-16 ***
## poly(income, 2)1   24.3320    10.4956   2.318   0.0204 *  
## poly(income, 2)2  -12.5346    11.2160  -1.118   0.2638    
## poly(balance, 2)1 262.7077    46.7667   5.617 1.94e-08 ***
## poly(balance, 2)2  14.8703    21.4391   0.694   0.4879    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1456.95  on 4999  degrees of freedom
## Residual deviance:  732.74  on 4995  degrees of freedom
## AIC: 742.74
## 
## Number of Fisher Scoring iterations: 10
```


### iii. Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default for that individual, and classifying the individual to the default category if the posterior probability is greater than 0.5.


```r
glm.pred1=predict(glm.fits.de1,default.test,type="response")
summary(glm.pred1)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000068 0.0002097 0.0016694 0.0352087 0.0128074 0.9659174
```

```r
glm.pred1=ifelse(glm.pred1<.5,"No","Yes")
summary(glm.pred1)
```

```
##    Length     Class      Mode 
##      5000 character character
```

```r
glm.pred2=predict(glm.fits.de2,default.test,type="response")
summary(glm.pred2)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000157 0.0003365 0.0019831 0.0348203 0.0130066 0.9752530
```

```r
glm.pred2=ifelse(glm.pred2<.5,"No","Yes")
summary(glm.pred2)
```

```
##    Length     Class      Mode 
##      5000 character character
```


### iv. Compute the validation set error, which is the fraction of the observations in the validation set that are misclassiﬁed.


```r
table(glm.pred1,obs=default.test$default)
```

```
##          obs
## glm.pred1   No  Yes
##       No  4805  115
##       Yes   28   52
```

```r
error=1-(mean(glm.pred1==default.test$default))
error
```

```
## [1] 0.0286
```

```r
table(glm.pred2,obs=default.test$default)
```

```
##          obs
## glm.pred2   No  Yes
##       No  4803  114
##       Yes   30   53
```

```r
error=1-(mean(glm.pred2==default.test$default))
error
```

```
## [1] 0.0288
```


## (c) Repeat the process in (b) three times, using three diﬀerent splits of the observations into a training set and a validation set. Comment on the results obtained.


```r
set.seed(2)
train=sample(10000,5000)
default.train <- Default[train,]
dim(default.train)
```

```
## [1] 5000    4
```

```r
default.test <- Default[-train,]
dim(default.test)
```

```
## [1] 5000    4
```

```r
attach(Default)
```

```
## The following objects are masked from Default (pos = 3):
## 
##     balance, default, income, student
```

```r
glm.fits.de1=glm(default~income+balance,data=Default,family=binomial,subset=train)
summary(glm.fits.de1)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = Default, subset = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2043  -0.1385  -0.0552  -0.0203   3.7058  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.184e+01  6.403e-01 -18.492  < 2e-16 ***
## income       2.717e-05  7.183e-06   3.783 0.000155 ***
## balance      5.703e-03  3.266e-04  17.460  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1443.44  on 4999  degrees of freedom
## Residual deviance:  776.64  on 4997  degrees of freedom
## AIC: 782.64
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.pred1=predict(glm.fits.de1,default.test,type="response")
summary(glm.pred1)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000093 0.0002935 0.0020417 0.0336895 0.0133762 0.9800218
```

```r
glm.pred1=ifelse(glm.pred1<.5,"No","Yes")
summary(glm.pred1)
```

```
##    Length     Class      Mode 
##      5000 character character
```

```r
table(glm.pred1,obs=default.test$default)
```

```
##          obs
## glm.pred1   No  Yes
##       No  4811  118
##       Yes   20   51
```

```r
error=1-(mean(glm.pred1==default.test$default))
error
```

```
## [1] 0.0276
```

```r
set.seed(3)
train=sample(10000,5000)
default.train <- Default[train,]
dim(default.train)
```

```
## [1] 5000    4
```

```r
default.test <- Default[-train,]
dim(default.test)
```

```
## [1] 5000    4
```

```r
attach(Default)
```

```
## The following objects are masked from Default (pos = 3):
## 
##     balance, default, income, student
```

```
## The following objects are masked from Default (pos = 4):
## 
##     balance, default, income, student
```

```r
glm.fits.de1=glm(default~income+balance,data=Default,family=binomial,subset=train)
summary(glm.fits.de1)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = Default, subset = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.1014  -0.1433  -0.0569  -0.0206   3.7241  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.160e+01  6.055e-01 -19.162  < 2e-16 ***
## income       2.254e-05  6.972e-06   3.233  0.00123 ** 
## balance      5.660e-03  3.131e-04  18.079  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1530.39  on 4999  degrees of freedom
## Residual deviance:  812.77  on 4997  degrees of freedom
## AIC: 818.77
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.pred1=predict(glm.fits.de1,default.test,type="response")
summary(glm.pred1)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000113 0.0003168 0.0020494 0.0318320 0.0133253 0.9759436
```

```r
glm.pred1=ifelse(glm.pred1<.5,"No","Yes")
summary(glm.pred1)
```

```
##    Length     Class      Mode 
##      5000 character character
```

```r
table(glm.pred1,obs=default.test$default)
```

```
##          obs
## glm.pred1   No  Yes
##       No  4828  108
##       Yes   16   48
```

```r
error=1-(mean(glm.pred1==default.test$default))
error
```

```
## [1] 0.0248
```

```r
set.seed(4)
train=sample(10000,5000)
default.train <- Default[train,]
dim(default.train)
```

```
## [1] 5000    4
```

```r
default.test <- Default[-train,]
dim(default.test)
```

```
## [1] 5000    4
```

```r
attach(Default)
```

```
## The following objects are masked from Default (pos = 3):
## 
##     balance, default, income, student
## 
## The following objects are masked from Default (pos = 4):
## 
##     balance, default, income, student
```

```
## The following objects are masked from Default (pos = 5):
## 
##     balance, default, income, student
```

```r
glm.fits.de1=glm(default~income+balance,data=Default,family=binomial,subset=train)
summary(glm.fits.de1)
```

```
## 
## Call:
## glm(formula = default ~ income + balance, family = binomial, 
##     data = Default, subset = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.4799  -0.1411  -0.0559  -0.0208   3.7223  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.156e+01  6.146e-01 -18.803  < 2e-16 ***
## income       2.004e-05  6.997e-06   2.864  0.00418 ** 
## balance      5.678e-03  3.244e-04  17.502  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1450.21  on 4999  degrees of freedom
## Residual deviance:  780.49  on 4997  degrees of freedom
## AIC: 786.49
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.pred1=predict(glm.fits.de1,default.test,type="response")
summary(glm.pred1)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000118 0.0002857 0.0021137 0.0339931 0.0135784 0.9811866
```

```r
glm.pred1=ifelse(glm.pred1<.5,"No","Yes")
summary(glm.pred1)
```

```
##    Length     Class      Mode 
##      5000 character character
```

```r
table(glm.pred1,obs=default.test$default)
```

```
##          obs
## glm.pred1   No  Yes
##       No  4813  112
##       Yes   19   56
```

```r
error=1-(mean(glm.pred1==default.test$default))
error
```

```
## [1] 0.0262
```


## (d) Now consider a logistic regression model that predicts the probability of default using income, balance, and a dummy variable for student. Estimate the test error for this model using the validation set approach. Comment on whether or not including a dummy variable for student leads to a reduction in the test error rate.


```r
summary(Default)
```

```
##  default    student       balance           income     
##  No :9667   No :7056   Min.   :   0.0   Min.   :  772  
##  Yes: 333   Yes:2944   1st Qu.: 481.7   1st Qu.:21340  
##                        Median : 823.6   Median :34553  
##                        Mean   : 835.4   Mean   :33517  
##                        3rd Qu.:1166.3   3rd Qu.:43808  
##                        Max.   :2654.3   Max.   :73554
```

```r
student01 = rep(0,10000)
student01[student=="Yes"]=1
summary(student01)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##  0.0000  0.0000  0.0000  0.2944  1.0000  1.0000
```

```r
str(student01)
```

```
##  num [1:10000] 0 1 0 0 0 1 0 1 0 0 ...
```

```r
new.Default <- cbind(Default, student01)
summary(new.Default)
```

```
##  default    student       balance           income        student01     
##  No :9667   No :7056   Min.   :   0.0   Min.   :  772   Min.   :0.0000  
##  Yes: 333   Yes:2944   1st Qu.: 481.7   1st Qu.:21340   1st Qu.:0.0000  
##                        Median : 823.6   Median :34553   Median :0.0000  
##                        Mean   : 835.4   Mean   :33517   Mean   :0.2944  
##                        3rd Qu.:1166.3   3rd Qu.:43808   3rd Qu.:1.0000  
##                        Max.   :2654.3   Max.   :73554   Max.   :1.0000
```

```r
set.seed(1)
train=sample(10000,5000)
default.train <- new.Default[train,]
dim(default.train)
```

```
## [1] 5000    5
```

```r
default.test <- new.Default[-train,]
dim(default.test)
```

```
## [1] 5000    5
```

```r
attach(new.Default)
```

```
## The following object is masked _by_ .GlobalEnv:
## 
##     student01
```

```
## The following objects are masked from Default (pos = 3):
## 
##     balance, default, income, student
```

```
## The following objects are masked from Default (pos = 4):
## 
##     balance, default, income, student
```

```
## The following objects are masked from Default (pos = 5):
## 
##     balance, default, income, student
```

```
## The following objects are masked from Default (pos = 6):
## 
##     balance, default, income, student
```

```r
glm.fits.de1=glm(default~income+balance+student01,data=new.Default,family=binomial,subset=train)
summary(glm.fits.de1)
```

```
## 
## Call:
## glm(formula = default ~ income + balance + student01, family = binomial, 
##     data = new.Default, subset = train)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.2905  -0.1260  -0.0465  -0.0161   3.7715  
## 
## Coefficients:
##               Estimate Std. Error z value Pr(>|z|)    
## (Intercept) -1.147e+01  7.562e-01 -15.164   <2e-16 ***
## income       2.433e-06  1.256e-05   0.194    0.846    
## balance      6.124e-03  3.525e-04  17.373   <2e-16 ***
## student01   -5.608e-01  3.473e-01  -1.615    0.106    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1456.95  on 4999  degrees of freedom
## Residual deviance:  731.81  on 4996  degrees of freedom
## AIC: 739.81
## 
## Number of Fisher Scoring iterations: 8
```

```r
glm.pred1=predict(glm.fits.de1,default.test,type="response")
summary(glm.pred1)
```

```
##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
## 0.0000061 0.0002013 0.0015709 0.0351380 0.0127782 0.9656062
```

```r
glm.pred1=ifelse(glm.pred1<.5,"No","Yes")
summary(glm.pred1)
```

```
##    Length     Class      Mode 
##      5000 character character
```

```r
table(glm.pred1,obs=default.test$default)
```

```
##          obs
## glm.pred1   No  Yes
##       No  4803  114
##       Yes   30   53
```

```r
error=1-(mean(glm.pred1==default.test$default))
error
```

```
## [1] 0.0288
```


# 7. In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be used in order to compute the LOOCV test error estimate. Alternatively, one could compute those quantities using just the glm() and predict.glm() functions, and a for loop. You will now take this approach in order to compute the LOOCV error for a simple logistic regression model on the Weekly data set. Recall that in the context of classiﬁcation problems, the LOOCV error is given in (5.4).

## (a) Fit a logistic regression model that predicts Direction using Lag1 and Lag2.


```r
summary(Weekly)
```

```
##       Year           Lag1               Lag2               Lag3         
##  Min.   :1990   Min.   :-18.1950   Min.   :-18.1950   Min.   :-18.1950  
##  1st Qu.:1995   1st Qu.: -1.1540   1st Qu.: -1.1540   1st Qu.: -1.1580  
##  Median :2000   Median :  0.2410   Median :  0.2410   Median :  0.2410  
##  Mean   :2000   Mean   :  0.1506   Mean   :  0.1511   Mean   :  0.1472  
##  3rd Qu.:2005   3rd Qu.:  1.4050   3rd Qu.:  1.4090   3rd Qu.:  1.4090  
##  Max.   :2010   Max.   : 12.0260   Max.   : 12.0260   Max.   : 12.0260  
##       Lag4               Lag5              Volume       
##  Min.   :-18.1950   Min.   :-18.1950   Min.   :0.08747  
##  1st Qu.: -1.1580   1st Qu.: -1.1660   1st Qu.:0.33202  
##  Median :  0.2380   Median :  0.2340   Median :1.00268  
##  Mean   :  0.1458   Mean   :  0.1399   Mean   :1.57462  
##  3rd Qu.:  1.4090   3rd Qu.:  1.4050   3rd Qu.:2.05373  
##  Max.   : 12.0260   Max.   : 12.0260   Max.   :9.32821  
##      Today          Direction 
##  Min.   :-18.1950   Down:484  
##  1st Qu.: -1.1540   Up  :605  
##  Median :  0.2410             
##  Mean   :  0.1499             
##  3rd Qu.:  1.4050             
##  Max.   : 12.0260
```

```r
glm.fits.Weekly=glm(Direction~Lag1+Lag2,data=Weekly,family=binomial)
summary(glm.fits.Weekly)
```

```
## 
## Call:
## glm(formula = Direction ~ Lag1 + Lag2, family = binomial, data = Weekly)
## 
## Deviance Residuals: 
##    Min      1Q  Median      3Q     Max  
## -1.623  -1.261   1.001   1.083   1.506  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.22122    0.06147   3.599 0.000319 ***
## Lag1        -0.03872    0.02622  -1.477 0.139672    
## Lag2         0.06025    0.02655   2.270 0.023232 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1496.2  on 1088  degrees of freedom
## Residual deviance: 1488.2  on 1086  degrees of freedom
## AIC: 1494.2
## 
## Number of Fisher Scoring iterations: 4
```


## (b) Fit a logistic regression model that predicts Direction using Lag1 and Lag2 using all but the ﬁrst observation.


```r
glm.fits.Weekly2=glm(Direction~Lag1+Lag2,data=Weekly[-1,],family=binomial)
summary(glm.fits.Weekly2)
```

```
## 
## Call:
## glm(formula = Direction ~ Lag1 + Lag2, family = binomial, data = Weekly[-1, 
##     ])
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.6258  -1.2617   0.9999   1.0819   1.5071  
## 
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  0.22324    0.06150   3.630 0.000283 ***
## Lag1        -0.03843    0.02622  -1.466 0.142683    
## Lag2         0.06085    0.02656   2.291 0.021971 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 1494.6  on 1087  degrees of freedom
## Residual deviance: 1486.5  on 1085  degrees of freedom
## AIC: 1492.5
## 
## Number of Fisher Scoring iterations: 4
```


## (c) Use the model from (b) to predict the direction of the ﬁrst observation. You can do this by predicting that the ﬁrst observation will go up if P (Direction="Up" | Lag1, Lag2) > 0.5. Was this observation correctly classiﬁed?


```r
predict(glm.fits.Weekly2,Weekly[1,],type="response")
```

```
##         1 
## 0.5713923
```

```r
Weekly[1,]$Direction
```

```
## [1] Down
## Levels: Down Up
```

> `0.57 > 0.5` up, wrong prediction

## (d) Write a for loop from i = 1 to i = n, where n is the number of observations in the data set, that performs each of the following steps:

### i. Fit a logistic regression model using all but the ith observation to predict Direction using Lag1 and Lag2.

### ii. Compute the posterior probability of the market moving up for the ith observation.

### iii. Use the posterior probability for the ith observation in order to predict whether or not the market moves up.

### iv. Determine whether or not an error was made in predicting the direction for the ith observation. If an error was made, then indicate this as a 1, and otherwise indicate it as a 0.


```r
str(Weekly)
```

```
## 'data.frame':	1089 obs. of  9 variables:
##  $ Year     : num  1990 1990 1990 1990 1990 1990 1990 1990 1990 1990 ...
##  $ Lag1     : num  0.816 -0.27 -2.576 3.514 0.712 ...
##  $ Lag2     : num  1.572 0.816 -0.27 -2.576 3.514 ...
##  $ Lag3     : num  -3.936 1.572 0.816 -0.27 -2.576 ...
##  $ Lag4     : num  -0.229 -3.936 1.572 0.816 -0.27 ...
##  $ Lag5     : num  -3.484 -0.229 -3.936 1.572 0.816 ...
##  $ Volume   : num  0.155 0.149 0.16 0.162 0.154 ...
##  $ Today    : num  -0.27 -2.576 3.514 0.712 1.178 ...
##  $ Direction: Factor w/ 2 levels "Down","Up": 1 1 2 2 2 1 2 2 2 1 ...
```

```r
attach(Weekly)

for (i in 1:1089){
glm.fits.Weeklyi=glm(Direction~Lag1+Lag2,data=Weekly[-i,],family=binomial)
predi=predict(glm.fits.Weeklyi,Weekly[i,],type="response")
predi=ifelse(predi>0.5,"Down","Up")
error[i]=ifelse(predi!=Weekly[i,]$Direction,1,0)
}
```



## (e) Take the average of the n numbers obtained in (d)iv in order to obtain the LOOCV estimate for the test error. Comment on the results.


```r
str(error)
```

```
##  num [1:1089] 0 0 1 0 1 0 1 1 1 0 ...
```

```r
mean(error)
```

```
## [1] 0.5500459
```

> error rate is 55%

# 8. We will now perform cross-validation on a simulated data set.

## (a) Generate a simulated data set as follows:


```r
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)
```

In this data set, what is n and what is p? Write out the model used to generate the data in equation form.

> n=100, p = 1

## (b) Create a scatterplot of X against Y . Comment on what you ﬁnd.


```r
plot(x,y)
```

![](chapter5-1_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


## (c) Set a random seed, and then compute the LOOCV errors that result from ﬁtting the following four models using least squares:

i. Y = β 0 + β 1 X + 
ii. Y = β 0 + β 1 X + β 2 X 2 + 
iii. Y = β 0 + β 1 X + β 2 X 2 + β 3 X 3 + 
iv. Y = β 0 + β 1 X + β 2 X 2 + β 3 X 3 + β 4 X 4 + .

Note you may ﬁnd it helpful to use the data.frame() function to create a single data set containing both X and Y .


```r
Data8 = data.frame(x,y)

summary(Data8)
```

```
##        x                 y            
##  Min.   :-2.2147   Min.   :-12.67519  
##  1st Qu.:-0.4942   1st Qu.: -2.34584  
##  Median : 0.1139   Median : -0.90394  
##  Mean   : 0.1089   Mean   : -1.55002  
##  3rd Qu.: 0.6915   3rd Qu.:  0.05108  
##  Max.   : 2.4016   Max.   :  2.33127
```

```r
str(Data8)
```

```
## 'data.frame':	100 obs. of  2 variables:
##  $ x: num  -0.626 0.184 -0.836 1.595 0.33 ...
##  $ y: num  -2.032 0.158 -3.143 -3.337 -0.542 ...
```

```r
library(boot)
set.seed(1)

cv.error=rep(0,4)
for (i in 1:4){
 glm.fit=glm(y~poly(x,i),data=Data8)
 cv.error[i]=cv.glm(Data8,glm.fit)$delta[1]
 }
cv.error
```

```
## [1] 7.2881616 0.9374236 0.9566218 0.9539049
```


## (d) Repeat (c) using another random seed, and report your results. Are your results the same as what you got in (c)? Why?


```r
set.seed(2)
cv.error=rep(0,4)
for (i in 1:4){
 glm.fit=glm(y~poly(x,i),data=Data8)
 cv.error[i]=cv.glm(Data8,glm.fit)$delta[1]
 }
cv.error
```

```
## [1] 7.2881616 0.9374236 0.9566218 0.9539049
```

> the same, because no random selection

## (e) Which of the models in (c) had the smallest LOOCV error? Is this what you expected? Explain your answer.

> the quadratic model had the smallest LOOCV error. Yes, because we use `y=x-2*x^2+rnorm(100)` to generate this data.

## (f) Comment on the statistical signiﬁcance of the coeﬃcient estimates that results from ﬁtting each of the models in (c) using least squares. Do these results agree with the conclusions drawn based on the cross-validation results?


```r
for (i in 1:4){
 glm.fit=glm(y~poly(x,i),data=Data8)
 print(summary(glm.fit))
 }
```

```
## 
## Call:
## glm(formula = y ~ poly(x, i), data = Data8)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -9.5161  -0.6800   0.6812   1.5491   3.8183  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)   -1.550      0.260  -5.961 3.95e-08 ***
## poly(x, i)     6.189      2.600   2.380   0.0192 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 6.760719)
## 
##     Null deviance: 700.85  on 99  degrees of freedom
## Residual deviance: 662.55  on 98  degrees of freedom
## AIC: 478.88
## 
## Number of Fisher Scoring iterations: 2
## 
## 
## Call:
## glm(formula = y ~ poly(x, i), data = Data8)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9650  -0.6254  -0.1288   0.5803   2.2700  
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.5500     0.0958  -16.18  < 2e-16 ***
## poly(x, i)1   6.1888     0.9580    6.46 4.18e-09 ***
## poly(x, i)2 -23.9483     0.9580  -25.00  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.9178258)
## 
##     Null deviance: 700.852  on 99  degrees of freedom
## Residual deviance:  89.029  on 97  degrees of freedom
## AIC: 280.17
## 
## Number of Fisher Scoring iterations: 2
## 
## 
## Call:
## glm(formula = y ~ poly(x, i), data = Data8)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.9765  -0.6302  -0.1227   0.5545   2.2843  
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.55002    0.09626 -16.102  < 2e-16 ***
## poly(x, i)1   6.18883    0.96263   6.429 4.97e-09 ***
## poly(x, i)2 -23.94830    0.96263 -24.878  < 2e-16 ***
## poly(x, i)3   0.26411    0.96263   0.274    0.784    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.9266599)
## 
##     Null deviance: 700.852  on 99  degrees of freedom
## Residual deviance:  88.959  on 96  degrees of freedom
## AIC: 282.09
## 
## Number of Fisher Scoring iterations: 2
## 
## 
## Call:
## glm(formula = y ~ poly(x, i), data = Data8)
## 
## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -2.0550  -0.6212  -0.1567   0.5952   2.2267  
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  -1.55002    0.09591 -16.162  < 2e-16 ***
## poly(x, i)1   6.18883    0.95905   6.453 4.59e-09 ***
## poly(x, i)2 -23.94830    0.95905 -24.971  < 2e-16 ***
## poly(x, i)3   0.26411    0.95905   0.275    0.784    
## poly(x, i)4   1.25710    0.95905   1.311    0.193    
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for gaussian family taken to be 0.9197797)
## 
##     Null deviance: 700.852  on 99  degrees of freedom
## Residual deviance:  87.379  on 95  degrees of freedom
## AIC: 282.3
## 
## Number of Fisher Scoring iterations: 2
```

