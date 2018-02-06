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
(a) Explain how k-fold cross-validation is implemented.
(b) What are the advantages and disadvantages of k-fold cross-
validation relative to:
i. The validation set approach?
ii. LOOCV?

# 5. In Chapter 4, we used logistic regression to predict the probability of
default using income and balance on the Default data set. We will
now estimate the test error of this logistic regression model using the
validation set approach. Do not forget to set a random seed before
beginning your analysis.
(a) Fit a logistic regression model that uses income and balance to
predict default.
(b) Using the validation set approach, estimate the test error of this
model. In order to do this, you must perform the following steps:
i. Split the sample set into a training set and a validation set.
ii. Fit a multiple logistic regression model using only the train-
ing observations.
iii. Obtain a prediction of default status for each individual in
the validation set by computing the posterior probability of
default for that individual, and classifying the individual to
the default category if the posterior probability is greater
than 0.5.
iv. Compute the validation set error, which is the fraction of
the observations in the validation set that are misclassiﬁed.
(c) Repeat the process in (b) three times, using three diﬀerent splits
of the observations into a training set and a validation set. Com-
ment on the results obtained.
(d) Now consider a logistic regression model that predicts the prob-
ability of default using income, balance, and a dummy variable
for student. Estimate the test error for this model using the val-
idation set approach. Comment on whether or not including a
dummy variable for student leads to a reduction in the test error
rate.

# 7. In Sections 5.3.2 and 5.3.3, we saw that the cv.glm() function can be
used in order to compute the LOOCV test error estimate. Alterna-
tively, one could compute those quantities using just the glm() and
predict.glm() functions, and a for loop. You will now take this ap-
proach in order to compute the LOOCV error for a simple logistic
regression model on the Weekly data set. Recall that in the context
of classiﬁcation problems, the LOOCV error is given in (5.4).
(a) Fit a logistic regression model that predicts Direction using Lag1
and Lag2.
(b) Fit a logistic regression model that predicts Direction using Lag1
and Lag2 using all but the ﬁrst observation.
(c) Use the model from (b) to predict the direction of the ﬁrst obser-
vation. You can do this by predicting that the ﬁrst observation
will go up if P (Direction="Up" | Lag1, Lag2) > 0.5. Was this ob-
servation correctly classiﬁed?
(d) Write a for loop from i = 1 to i = n, where n is the number of
observations in the data set, that performs each of the following
steps:
i. Fit a logistic regression model using all but the ith obser-
vation to predict Direction using Lag1 and Lag2.
ii. Compute the posterior probability of the market moving up
for the ith observation.
iii. Use the posterior probability for the ith observation in order
to predict whether or not the market moves up.
iv. Determine whether or not an error was made in predicting
the direction for the ith observation. If an error was made,
then indicate this as a 1, and otherwise indicate it as a 0.
(e) Take the average of the n numbers obtained in (d)iv in order to
obtain the LOOCV estimate for the test error. Comment on the
results.

# 8. We will now perform cross-validation on a simulated data set.
(a) Generate a simulated data set as follows:
> set .seed (1)
> x= rnorm (100)
> y=x -2* x^2+ rnorm (100)
In this data set, what is n and what is p? Write out the model
used to generate the data in equation form.
(b) Create a scatterplot of X against Y . Comment on what you ﬁnd.
(c) Set a random seed, and then compute the LOOCV errors that
result from ﬁtting the following four models using least squares:
i. Y = β 0 + β 1 X + 
ii. Y = β 0 + β 1 X + β 2 X 2 + 
iii. Y = β 0 + β 1 X + β 2 X 2 + β 3 X 3 + 
iv. Y = β 0 + β 1 X + β 2 X 2 + β 3 X 3 + β 4 X 4 + .
Note you may ﬁnd it helpful to use the data.frame() function
to create a single data set containing both X and Y .
(d) Repeat (c) using another random seed, and report your results.
Are your results the same as what you got in (c)? Why?
(e) Which of the models in (c) had the smallest LOOCV error? Is
this what you expected? Explain your answer.
(f) Comment on the statistical signiﬁcance of the coeﬃcient esti-
mates that results from ﬁtting each of the models in (c) using
least squares. Do these results agree with the conclusions drawn
based on the cross-validation results?
