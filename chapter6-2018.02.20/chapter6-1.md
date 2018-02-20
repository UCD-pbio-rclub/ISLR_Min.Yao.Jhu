# chapter6-1
Min-Yao  
2018年2月18日  

# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection


```r
library(ISLR)
#fix(Hitters)
names(Hitters)
```

```
##  [1] "AtBat"     "Hits"      "HmRun"     "Runs"      "RBI"      
##  [6] "Walks"     "Years"     "CAtBat"    "CHits"     "CHmRun"   
## [11] "CRuns"     "CRBI"      "CWalks"    "League"    "Division" 
## [16] "PutOuts"   "Assists"   "Errors"    "Salary"    "NewLeague"
```

```r
dim(Hitters)
```

```
## [1] 322  20
```

```r
sum(is.na(Hitters$Salary))
```

```
## [1] 59
```

```r
Hitters=na.omit(Hitters)
dim(Hitters)
```

```
## [1] 263  20
```

```r
sum(is.na(Hitters))
```

```
## [1] 0
```

```r
library(leaps)
regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
```

```
## Subset selection object
## Call: regsubsets.formula(Salary ~ ., Hitters)
## 19 Variables  (and intercept)
##            Forced in Forced out
## AtBat          FALSE      FALSE
## Hits           FALSE      FALSE
## HmRun          FALSE      FALSE
## Runs           FALSE      FALSE
## RBI            FALSE      FALSE
## Walks          FALSE      FALSE
## Years          FALSE      FALSE
## CAtBat         FALSE      FALSE
## CHits          FALSE      FALSE
## CHmRun         FALSE      FALSE
## CRuns          FALSE      FALSE
## CRBI           FALSE      FALSE
## CWalks         FALSE      FALSE
## LeagueN        FALSE      FALSE
## DivisionW      FALSE      FALSE
## PutOuts        FALSE      FALSE
## Assists        FALSE      FALSE
## Errors         FALSE      FALSE
## NewLeagueN     FALSE      FALSE
## 1 subsets of each size up to 8
## Selection Algorithm: exhaustive
##          AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns
## 1  ( 1 ) " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 2  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 3  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 4  ( 1 ) " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 5  ( 1 ) "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 6  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "  
## 7  ( 1 ) " "   "*"  " "   " "  " " "*"   " "   "*"    "*"   "*"    " "  
## 8  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   " "    " "   "*"    "*"  
##          CRBI CWalks LeagueN DivisionW PutOuts Assists Errors NewLeagueN
## 1  ( 1 ) "*"  " "    " "     " "       " "     " "     " "    " "       
## 2  ( 1 ) "*"  " "    " "     " "       " "     " "     " "    " "       
## 3  ( 1 ) "*"  " "    " "     " "       "*"     " "     " "    " "       
## 4  ( 1 ) "*"  " "    " "     "*"       "*"     " "     " "    " "       
## 5  ( 1 ) "*"  " "    " "     "*"       "*"     " "     " "    " "       
## 6  ( 1 ) "*"  " "    " "     "*"       "*"     " "     " "    " "       
## 7  ( 1 ) " "  " "    " "     "*"       "*"     " "     " "    " "       
## 8  ( 1 ) " "  "*"    " "     "*"       "*"     " "     " "    " "
```

```r
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
reg.summary=summary(regfit.full)
names(reg.summary)
```

```
## [1] "which"  "rsq"    "rss"    "adjr2"  "cp"     "bic"    "outmat" "obj"
```

```r
reg.summary$rsq
```

```
##  [1] 0.3214501 0.4252237 0.4514294 0.4754067 0.4908036 0.5087146 0.5141227
##  [8] 0.5285569 0.5346124 0.5404950 0.5426153 0.5436302 0.5444570 0.5452164
## [15] 0.5454692 0.5457656 0.5459518 0.5460945 0.5461159
```

```r
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2)
```

```
## [1] 11
```

```r
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20)

plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
```

```
## [1] 10
```

```r
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
```

```
## [1] 6
```

```r
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)
```

![](chapter6-1_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
#?plot.regsubsets
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
```

![](chapter6-1_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
coef(regfit.full,6)
```

```
##  (Intercept)        AtBat         Hits        Walks         CRBI 
##   91.5117981   -1.8685892    7.6043976    3.6976468    0.6430169 
##    DivisionW      PutOuts 
## -122.9515338    0.2643076
```


# Forward and Backward Stepwise Selection


```r
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
```

```
## Subset selection object
## Call: regsubsets.formula(Salary ~ ., data = Hitters, nvmax = 19, method = "forward")
## 19 Variables  (and intercept)
##            Forced in Forced out
## AtBat          FALSE      FALSE
## Hits           FALSE      FALSE
## HmRun          FALSE      FALSE
## Runs           FALSE      FALSE
## RBI            FALSE      FALSE
## Walks          FALSE      FALSE
## Years          FALSE      FALSE
## CAtBat         FALSE      FALSE
## CHits          FALSE      FALSE
## CHmRun         FALSE      FALSE
## CRuns          FALSE      FALSE
## CRBI           FALSE      FALSE
## CWalks         FALSE      FALSE
## LeagueN        FALSE      FALSE
## DivisionW      FALSE      FALSE
## PutOuts        FALSE      FALSE
## Assists        FALSE      FALSE
## Errors         FALSE      FALSE
## NewLeagueN     FALSE      FALSE
## 1 subsets of each size up to 19
## Selection Algorithm: forward
##           AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns
## 1  ( 1 )  " "   " "  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 2  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 3  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 4  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 5  ( 1 )  "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    " "  
## 6  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "  
## 7  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    " "  
## 8  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"  
## 9  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"  
## 10  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"  
## 11  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"  
## 12  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"  
## 13  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"  
## 14  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    " "   " "    "*"  
## 15  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    "*"   " "    "*"  
## 16  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"  
## 17  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"  
## 18  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   " "    "*"  
## 19  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   "*"    "*"  
##           CRBI CWalks LeagueN DivisionW PutOuts Assists Errors NewLeagueN
## 1  ( 1 )  "*"  " "    " "     " "       " "     " "     " "    " "       
## 2  ( 1 )  "*"  " "    " "     " "       " "     " "     " "    " "       
## 3  ( 1 )  "*"  " "    " "     " "       "*"     " "     " "    " "       
## 4  ( 1 )  "*"  " "    " "     "*"       "*"     " "     " "    " "       
## 5  ( 1 )  "*"  " "    " "     "*"       "*"     " "     " "    " "       
## 6  ( 1 )  "*"  " "    " "     "*"       "*"     " "     " "    " "       
## 7  ( 1 )  "*"  "*"    " "     "*"       "*"     " "     " "    " "       
## 8  ( 1 )  "*"  "*"    " "     "*"       "*"     " "     " "    " "       
## 9  ( 1 )  "*"  "*"    " "     "*"       "*"     " "     " "    " "       
## 10  ( 1 ) "*"  "*"    " "     "*"       "*"     "*"     " "    " "       
## 11  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     " "    " "       
## 12  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     " "    " "       
## 13  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    " "       
## 14  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    " "       
## 15  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    " "       
## 16  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    " "       
## 17  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    "*"       
## 18  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    "*"       
## 19  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    "*"
```

```r
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
```

```
## Subset selection object
## Call: regsubsets.formula(Salary ~ ., data = Hitters, nvmax = 19, method = "backward")
## 19 Variables  (and intercept)
##            Forced in Forced out
## AtBat          FALSE      FALSE
## Hits           FALSE      FALSE
## HmRun          FALSE      FALSE
## Runs           FALSE      FALSE
## RBI            FALSE      FALSE
## Walks          FALSE      FALSE
## Years          FALSE      FALSE
## CAtBat         FALSE      FALSE
## CHits          FALSE      FALSE
## CHmRun         FALSE      FALSE
## CRuns          FALSE      FALSE
## CRBI           FALSE      FALSE
## CWalks         FALSE      FALSE
## LeagueN        FALSE      FALSE
## DivisionW      FALSE      FALSE
## PutOuts        FALSE      FALSE
## Assists        FALSE      FALSE
## Errors         FALSE      FALSE
## NewLeagueN     FALSE      FALSE
## 1 subsets of each size up to 19
## Selection Algorithm: backward
##           AtBat Hits HmRun Runs RBI Walks Years CAtBat CHits CHmRun CRuns
## 1  ( 1 )  " "   " "  " "   " "  " " " "   " "   " "    " "   " "    "*"  
## 2  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    "*"  
## 3  ( 1 )  " "   "*"  " "   " "  " " " "   " "   " "    " "   " "    "*"  
## 4  ( 1 )  "*"   "*"  " "   " "  " " " "   " "   " "    " "   " "    "*"  
## 5  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"  
## 6  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"  
## 7  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"  
## 8  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   " "    " "   " "    "*"  
## 9  ( 1 )  "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"  
## 10  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"  
## 11  ( 1 ) "*"   "*"  " "   " "  " " "*"   " "   "*"    " "   " "    "*"  
## 12  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"  
## 13  ( 1 ) "*"   "*"  " "   "*"  " " "*"   " "   "*"    " "   " "    "*"  
## 14  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    " "   " "    "*"  
## 15  ( 1 ) "*"   "*"  "*"   "*"  " " "*"   " "   "*"    "*"   " "    "*"  
## 16  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"  
## 17  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   " "   "*"    "*"   " "    "*"  
## 18  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   " "    "*"  
## 19  ( 1 ) "*"   "*"  "*"   "*"  "*" "*"   "*"   "*"    "*"   "*"    "*"  
##           CRBI CWalks LeagueN DivisionW PutOuts Assists Errors NewLeagueN
## 1  ( 1 )  " "  " "    " "     " "       " "     " "     " "    " "       
## 2  ( 1 )  " "  " "    " "     " "       " "     " "     " "    " "       
## 3  ( 1 )  " "  " "    " "     " "       "*"     " "     " "    " "       
## 4  ( 1 )  " "  " "    " "     " "       "*"     " "     " "    " "       
## 5  ( 1 )  " "  " "    " "     " "       "*"     " "     " "    " "       
## 6  ( 1 )  " "  " "    " "     "*"       "*"     " "     " "    " "       
## 7  ( 1 )  " "  "*"    " "     "*"       "*"     " "     " "    " "       
## 8  ( 1 )  "*"  "*"    " "     "*"       "*"     " "     " "    " "       
## 9  ( 1 )  "*"  "*"    " "     "*"       "*"     " "     " "    " "       
## 10  ( 1 ) "*"  "*"    " "     "*"       "*"     "*"     " "    " "       
## 11  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     " "    " "       
## 12  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     " "    " "       
## 13  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    " "       
## 14  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    " "       
## 15  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    " "       
## 16  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    " "       
## 17  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    "*"       
## 18  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    "*"       
## 19  ( 1 ) "*"  "*"    "*"     "*"       "*"     "*"     "*"    "*"
```

```r
coef(regfit.full,7)
```

```
##  (Intercept)         Hits        Walks       CAtBat        CHits 
##   79.4509472    1.2833513    3.2274264   -0.3752350    1.4957073 
##       CHmRun    DivisionW      PutOuts 
##    1.4420538 -129.9866432    0.2366813
```

```r
coef(regfit.fwd,7)
```

```
##  (Intercept)        AtBat         Hits        Walks         CRBI 
##  109.7873062   -1.9588851    7.4498772    4.9131401    0.8537622 
##       CWalks    DivisionW      PutOuts 
##   -0.3053070 -127.1223928    0.2533404
```

```r
coef(regfit.bwd,7)
```

```
##  (Intercept)        AtBat         Hits        Walks        CRuns 
##  105.6487488   -1.9762838    6.7574914    6.0558691    1.1293095 
##       CWalks    DivisionW      PutOuts 
##   -0.7163346 -116.1692169    0.3028847
```

# 1. We perform best subset, forward stepwise, and backward stepwise selection on a single data set. For each approach, we obtain p + 1 models, containing 0, 1, 2, . . . , p predictors. Explain your answers:

## (a) Which of the three models with k predictors has the smallest training RSS?

> Best subset, because forward stepwise and backward stepwise selection are not guaranteed to ﬁnd the best possible model out of all 2^p models containing subsets of the p predictors. 

## (b) Which of the three models with k predictors has the smallest test RSS?

> We don't know. A number of techniques for adjusting the training error for the model size are available, like Cp, BIC, AIC and adjusted R^2. These approaches can be used to select among a set of models with diﬀerent numbers of variables. But, actually, we don't know which model has the real smallest test RSS unless we have real test data.

## (c) True or False:

### i. The predictors in the k-variable model identiﬁed by forward stepwise are a subset of the predictors in the (k+1)-variable model identiﬁed by forward stepwise selection.

> True.

### ii. The predictors in the k-variable model identiﬁed by backward stepwise are a subset of the predictors in the (k + 1)-variable model identiﬁed by backward stepwise selection.

> True.

### iii. The predictors in the k-variable model identiﬁed by back-ward stepwise are a subset of the predictors in the (k + 1)-variable model identiﬁed by forward stepwise selection.

> False. The best k-variable models and (k + 1)-variable models identiﬁed by forward stepwise selection and backward stepwise selection may be diﬀerent.

### iv. The predictors in the k-variable model identiﬁed by forward stepwise are a subset of the predictors in the (k+1)-variable model identiﬁed by backward stepwise selection.

> False. The best k-variable models and (k + 1)-variable models identiﬁed by forward stepwise selection and backward stepwise selection may be diﬀerent.

### v. The predictors in the k-variable model identiﬁed by best subset are a subset of the predictors in the (k + 1)-variable model identiﬁed by best subset selection.

> False. Because best subset may also remove any variables that no longer provide an improvement in the model ﬁt.

# 8. In this exercise, we will generate simulated data, and will then use this data to perform best subset selection.

## (a) Use the rnorm() function to generate a predictor X of length n = 100, as well as a noise vector E of length n = 100.


```r
set.seed(12)
#?rnorm
X=rnorm(100, 10, 1)
E=rnorm(100, 5, 1)
```


## (b) Generate a response vector Y of length n = 100 according to the model Y = β 0 + β 1 X + β 2 X^2 + β 3 X^3 + E, where β 0 , β 1 , β 2 , and β 3 are constants of your choice.


```r
B0=1
B1=2
B2=3
B3=4
Y=B0+B1*X+B2*(X^2)+B3*(X^3)+E
```


## (c) Use the regsubsets() function to perform best subset selection in order to choose the best model containing the predictors X, X^2, . . . , X^10. What is the best model obtained according to Cp , BIC, and adjusted R 2 ? Show some plots to provide evidence for your answer, and report the coeﬃcients of the best model obtained. Note you will need to use the data.frame() function to create a single data set containing both X and Y.


```r
library(leaps)
XYdata=data.frame(X,Y)
summary(XYdata)
```

```
##        X                Y       
##  Min.   : 7.851   Min.   :2144  
##  1st Qu.: 9.437   1st Qu.:3654  
##  Median : 9.890   Median :4189  
##  Mean   : 9.969   Mean   :4378  
##  3rd Qu.:10.509   3rd Qu.:5002  
##  Max.   :12.072   Max.   :7505
```

```r
plot(X,Y)
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
XYregfit.full=regsubsets(Y~poly(X,10),XYdata,nvmax = 10)
summary(XYregfit.full)
```

```
## Subset selection object
## Call: regsubsets.formula(Y ~ poly(X, 10), XYdata, nvmax = 10)
## 10 Variables  (and intercept)
##               Forced in Forced out
## poly(X, 10)1      FALSE      FALSE
## poly(X, 10)2      FALSE      FALSE
## poly(X, 10)3      FALSE      FALSE
## poly(X, 10)4      FALSE      FALSE
## poly(X, 10)5      FALSE      FALSE
## poly(X, 10)6      FALSE      FALSE
## poly(X, 10)7      FALSE      FALSE
## poly(X, 10)8      FALSE      FALSE
## poly(X, 10)9      FALSE      FALSE
## poly(X, 10)10     FALSE      FALSE
## 1 subsets of each size up to 10
## Selection Algorithm: exhaustive
##           poly(X, 10)1 poly(X, 10)2 poly(X, 10)3 poly(X, 10)4 poly(X, 10)5
## 1  ( 1 )  "*"          " "          " "          " "          " "         
## 2  ( 1 )  "*"          "*"          " "          " "          " "         
## 3  ( 1 )  "*"          "*"          "*"          " "          " "         
## 4  ( 1 )  "*"          "*"          "*"          " "          " "         
## 5  ( 1 )  "*"          "*"          "*"          "*"          " "         
## 6  ( 1 )  "*"          "*"          "*"          "*"          " "         
## 7  ( 1 )  "*"          "*"          "*"          "*"          "*"         
## 8  ( 1 )  "*"          "*"          "*"          "*"          "*"         
## 9  ( 1 )  "*"          "*"          "*"          "*"          "*"         
## 10  ( 1 ) "*"          "*"          "*"          "*"          "*"         
##           poly(X, 10)6 poly(X, 10)7 poly(X, 10)8 poly(X, 10)9
## 1  ( 1 )  " "          " "          " "          " "         
## 2  ( 1 )  " "          " "          " "          " "         
## 3  ( 1 )  " "          " "          " "          " "         
## 4  ( 1 )  " "          " "          " "          " "         
## 5  ( 1 )  " "          " "          " "          " "         
## 6  ( 1 )  " "          "*"          " "          " "         
## 7  ( 1 )  " "          "*"          " "          " "         
## 8  ( 1 )  " "          "*"          "*"          " "         
## 9  ( 1 )  "*"          "*"          "*"          " "         
## 10  ( 1 ) "*"          "*"          "*"          "*"         
##           poly(X, 10)10
## 1  ( 1 )  " "          
## 2  ( 1 )  " "          
## 3  ( 1 )  " "          
## 4  ( 1 )  "*"          
## 5  ( 1 )  "*"          
## 6  ( 1 )  "*"          
## 7  ( 1 )  "*"          
## 8  ( 1 )  "*"          
## 9  ( 1 )  "*"          
## 10  ( 1 ) "*"
```

```r
plot(XYregfit.full,scale="r2")
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

```r
plot(XYregfit.full,scale="adjr2")
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

```r
plot(XYregfit.full,scale="Cp")
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

```r
plot(XYregfit.full,scale="bic")
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-5.png)<!-- -->

```r
XYreg.summary=summary(XYregfit.full)
plot(XYreg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-6.png)<!-- -->

```r
plot(XYreg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(XYreg.summary$cp)
```

```
## [1] 4
```

```r
points(which.min(XYreg.summary$cp),XYreg.summary$cp[which.min(XYreg.summary$cp)],col="red",cex=2,pch=20)
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-7.png)<!-- -->

```r
plot(XYreg.summary$bic,xlab="Number of Variables",ylab="bic",type='l')
which.min(XYreg.summary$bic)
```

```
## [1] 3
```

```r
points(which.min(XYreg.summary$bic),XYreg.summary$bic[which.min(XYreg.summary$bic)],col="red",cex=2,pch=20)
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-8.png)<!-- -->

```r
plot(XYreg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(XYreg.summary$adjr2)
```

```
## [1] 4
```

```r
points(which.max(XYreg.summary$adjr2),XYreg.summary$adjr2[which.max(XYreg.summary$adjr2)], col="red",cex=2,pch=20)
```

![](chapter6-1_files/figure-html/unnamed-chunk-5-9.png)<!-- -->

```r
coef(XYregfit.full,3)
```

```
##  (Intercept) poly(X, 10)1 poly(X, 10)2 poly(X, 10)3 
##   4378.10082  11032.72208   1271.98179     49.78343
```

```r
coef(XYregfit.full,4)
```

```
##   (Intercept)  poly(X, 10)1  poly(X, 10)2  poly(X, 10)3 poly(X, 10)10 
##   4378.100817  11032.722081   1271.981792     49.783432      1.898334
```

> Cp and Adjusted RSq favor 4 variable model, but BIC favors 3 variable model.

## (d) Repeat (c), using forward stepwise selection and also using backwards stepwise selection. How does your answer compare to the results in (c)?


```r
XYregfit.fwd=regsubsets(Y~poly(X,10),XYdata,nvmax = 10,method="forward")
summary(XYregfit.fwd)
```

```
## Subset selection object
## Call: regsubsets.formula(Y ~ poly(X, 10), XYdata, nvmax = 10, method = "forward")
## 10 Variables  (and intercept)
##               Forced in Forced out
## poly(X, 10)1      FALSE      FALSE
## poly(X, 10)2      FALSE      FALSE
## poly(X, 10)3      FALSE      FALSE
## poly(X, 10)4      FALSE      FALSE
## poly(X, 10)5      FALSE      FALSE
## poly(X, 10)6      FALSE      FALSE
## poly(X, 10)7      FALSE      FALSE
## poly(X, 10)8      FALSE      FALSE
## poly(X, 10)9      FALSE      FALSE
## poly(X, 10)10     FALSE      FALSE
## 1 subsets of each size up to 10
## Selection Algorithm: forward
##           poly(X, 10)1 poly(X, 10)2 poly(X, 10)3 poly(X, 10)4 poly(X, 10)5
## 1  ( 1 )  "*"          " "          " "          " "          " "         
## 2  ( 1 )  "*"          "*"          " "          " "          " "         
## 3  ( 1 )  "*"          "*"          "*"          " "          " "         
## 4  ( 1 )  "*"          "*"          "*"          " "          " "         
## 5  ( 1 )  "*"          "*"          "*"          "*"          " "         
## 6  ( 1 )  "*"          "*"          "*"          "*"          " "         
## 7  ( 1 )  "*"          "*"          "*"          "*"          "*"         
## 8  ( 1 )  "*"          "*"          "*"          "*"          "*"         
## 9  ( 1 )  "*"          "*"          "*"          "*"          "*"         
## 10  ( 1 ) "*"          "*"          "*"          "*"          "*"         
##           poly(X, 10)6 poly(X, 10)7 poly(X, 10)8 poly(X, 10)9
## 1  ( 1 )  " "          " "          " "          " "         
## 2  ( 1 )  " "          " "          " "          " "         
## 3  ( 1 )  " "          " "          " "          " "         
## 4  ( 1 )  " "          " "          " "          " "         
## 5  ( 1 )  " "          " "          " "          " "         
## 6  ( 1 )  " "          "*"          " "          " "         
## 7  ( 1 )  " "          "*"          " "          " "         
## 8  ( 1 )  " "          "*"          "*"          " "         
## 9  ( 1 )  "*"          "*"          "*"          " "         
## 10  ( 1 ) "*"          "*"          "*"          "*"         
##           poly(X, 10)10
## 1  ( 1 )  " "          
## 2  ( 1 )  " "          
## 3  ( 1 )  " "          
## 4  ( 1 )  "*"          
## 5  ( 1 )  "*"          
## 6  ( 1 )  "*"          
## 7  ( 1 )  "*"          
## 8  ( 1 )  "*"          
## 9  ( 1 )  "*"          
## 10  ( 1 ) "*"
```

```r
plot(XYregfit.fwd,scale="r2")
```

![](chapter6-1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
plot(XYregfit.fwd,scale="adjr2")
```

![](chapter6-1_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

```r
plot(XYregfit.fwd,scale="Cp")
```

![](chapter6-1_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

```r
plot(XYregfit.fwd,scale="bic")
```

![](chapter6-1_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

```r
XYreg.summary=summary(XYregfit.fwd)
plot(XYreg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
```

![](chapter6-1_files/figure-html/unnamed-chunk-6-5.png)<!-- -->

```r
plot(XYreg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(XYreg.summary$cp)
```

```
## [1] 4
```

```r
points(which.min(XYreg.summary$cp),XYreg.summary$cp[which.min(XYreg.summary$cp)],col="red",cex=2,pch=20)
```

![](chapter6-1_files/figure-html/unnamed-chunk-6-6.png)<!-- -->

```r
plot(XYreg.summary$bic,xlab="Number of Variables",ylab="bic",type='l')
which.min(XYreg.summary$bic)
```

```
## [1] 3
```

```r
points(which.min(XYreg.summary$bic),XYreg.summary$bic[which.min(XYreg.summary$bic)],col="red",cex=2,pch=20)
```

![](chapter6-1_files/figure-html/unnamed-chunk-6-7.png)<!-- -->

```r
plot(XYreg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(XYreg.summary$adjr2)
```

```
## [1] 4
```

```r
points(which.max(XYreg.summary$adjr2),XYreg.summary$adjr2[which.max(XYreg.summary$adjr2)], col="red",cex=2,pch=20)
```

![](chapter6-1_files/figure-html/unnamed-chunk-6-8.png)<!-- -->

> the same results



## (e) Now ﬁt a lasso model to the simulated data, again using X, X^2 , . . . , X^10 as predictors. Use cross-validation to select the optimal value of λ. Create plots of the cross-validation error as a function of λ. Report the resulting coeﬃcient estimates, and discuss the results obtained.

## (f) Now generate a response vector Y according to the model Y = β 0 + β 7 X^7 + E, and perform best subset selection and the lasso. Discuss the results obtained.
