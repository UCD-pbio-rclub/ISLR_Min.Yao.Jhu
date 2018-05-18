---
title: "chapter10-1"
author: "Min-Yao"
date: "2018年5月17日"
output: 
  html_document: 
    keep_md: yes
---

# Chapter 10 Lab 1: Principal Components Analysis


```r
states=row.names(USArrests)
states
```

```
##  [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"      
##  [5] "California"     "Colorado"       "Connecticut"    "Delaware"      
##  [9] "Florida"        "Georgia"        "Hawaii"         "Idaho"         
## [13] "Illinois"       "Indiana"        "Iowa"           "Kansas"        
## [17] "Kentucky"       "Louisiana"      "Maine"          "Maryland"      
## [21] "Massachusetts"  "Michigan"       "Minnesota"      "Mississippi"   
## [25] "Missouri"       "Montana"        "Nebraska"       "Nevada"        
## [29] "New Hampshire"  "New Jersey"     "New Mexico"     "New York"      
## [33] "North Carolina" "North Dakota"   "Ohio"           "Oklahoma"      
## [37] "Oregon"         "Pennsylvania"   "Rhode Island"   "South Carolina"
## [41] "South Dakota"   "Tennessee"      "Texas"          "Utah"          
## [45] "Vermont"        "Virginia"       "Washington"     "West Virginia" 
## [49] "Wisconsin"      "Wyoming"
```

```r
names(USArrests)
```

```
## [1] "Murder"   "Assault"  "UrbanPop" "Rape"
```

```r
dimnames(USArrests)
```

```
## [[1]]
##  [1] "Alabama"        "Alaska"         "Arizona"        "Arkansas"      
##  [5] "California"     "Colorado"       "Connecticut"    "Delaware"      
##  [9] "Florida"        "Georgia"        "Hawaii"         "Idaho"         
## [13] "Illinois"       "Indiana"        "Iowa"           "Kansas"        
## [17] "Kentucky"       "Louisiana"      "Maine"          "Maryland"      
## [21] "Massachusetts"  "Michigan"       "Minnesota"      "Mississippi"   
## [25] "Missouri"       "Montana"        "Nebraska"       "Nevada"        
## [29] "New Hampshire"  "New Jersey"     "New Mexico"     "New York"      
## [33] "North Carolina" "North Dakota"   "Ohio"           "Oklahoma"      
## [37] "Oregon"         "Pennsylvania"   "Rhode Island"   "South Carolina"
## [41] "South Dakota"   "Tennessee"      "Texas"          "Utah"          
## [45] "Vermont"        "Virginia"       "Washington"     "West Virginia" 
## [49] "Wisconsin"      "Wyoming"       
## 
## [[2]]
## [1] "Murder"   "Assault"  "UrbanPop" "Rape"
```

```r
apply(USArrests, 2, mean)
```

```
##   Murder  Assault UrbanPop     Rape 
##    7.788  170.760   65.540   21.232
```

```r
apply(USArrests, 2, var)
```

```
##     Murder    Assault   UrbanPop       Rape 
##   18.97047 6945.16571  209.51878   87.72916
```

```r
pr.out=prcomp(USArrests, scale=TRUE)
pr.out
```

```
## Standard deviations (1, .., p=4):
## [1] 1.5748783 0.9948694 0.5971291 0.4164494
## 
## Rotation (n x k) = (4 x 4):
##                 PC1        PC2        PC3         PC4
## Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
## Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
## UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
## Rape     -0.5434321 -0.1673186  0.8177779  0.08902432
```

```r
names(pr.out)
```

```
## [1] "sdev"     "rotation" "center"   "scale"    "x"
```

```r
pr.out$center
```

```
##   Murder  Assault UrbanPop     Rape 
##    7.788  170.760   65.540   21.232
```

```r
pr.out$scale
```

```
##    Murder   Assault  UrbanPop      Rape 
##  4.355510 83.337661 14.474763  9.366385
```

```r
pr.out$rotation
```

```
##                 PC1        PC2        PC3         PC4
## Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
## Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
## UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
## Rape     -0.5434321 -0.1673186  0.8177779  0.08902432
```

```r
dim(pr.out$x)
```

```
## [1] 50  4
```

```r
biplot(pr.out, scale=0)
```

![](chapter10-1_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

```r
biplot(pr.out, scale=0, cex=0.6)
```

![](chapter10-1_files/figure-html/unnamed-chunk-1-2.png)<!-- -->

```r
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
```

![](chapter10-1_files/figure-html/unnamed-chunk-1-3.png)<!-- -->

```r
biplot(pr.out, scale=0, cex=0.6)
```

![](chapter10-1_files/figure-html/unnamed-chunk-1-4.png)<!-- -->

```r
pr.out$sdev
```

```
## [1] 1.5748783 0.9948694 0.5971291 0.4164494
```

```r
pr.var=pr.out$sdev^2
pr.var
```

```
## [1] 2.4802416 0.9897652 0.3565632 0.1734301
```

```r
pve=pr.var/sum(pr.var)
pve
```

```
## [1] 0.62006039 0.24744129 0.08914080 0.04335752
```

```r
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
```

![](chapter10-1_files/figure-html/unnamed-chunk-1-5.png)<!-- -->

```r
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
```

![](chapter10-1_files/figure-html/unnamed-chunk-1-6.png)<!-- -->

```r
a=c(1,2,8,-3)
cumsum(a)
```

```
## [1]  1  3 11  8
```

# 6. A researcher collects expression measurements for 1,000 genes in 100 tissue samples. The data can be written as a 1, 000 × 100 matrix, which we call X, in which each row represents a gene and each column a tissue sample. Each tissue sample was processed on a diﬀerent day, and the columns of X are ordered so that the samples that were processed earliest are on the left, and the samples that were processed later are on the right. The tissue samples belong to two groups: control (C) and treatment (T). The C and T samples were processed in a random order across the days. The researcher wishes to determine whether each gene’s expression measurements diﬀer between the treatment and control groups. 
As a pre-analysis (before comparing T versus C), the researcher performs a principal component analysis of the data, and ﬁnds that the ﬁrst principal component (a vector of length 100) has a strong linear trend from left to right, and explains 10 % of the variation. The researcher now remembers that each patient sample was run on one of
two machines, A and B, and machine A was used more often in the earlier times while B was used more often later. The researcher has a record of which sample was run on which machine.

## (a) Explain what it means that the ﬁrst principal component “explains 10 % of the variation”.

> The proportion of variance explained (PVE) by each principal component. This means PC1 contains 10 % of total variance present in this data set.

## (b) The researcher decides to replace the (j, i)th element of X with
x ji − φ j1 z i1
where z i1 is the ith score, and φ j1 is the jth loading, for the ﬁrst principal component. He will then perform a two-sample t-test on each gene in this new data set in order to determine whether its expression diﬀers between the two conditions. Critique this idea, and suggest a better approach. (The principal component analysis is performed on X T ).

> Since each patient sample was run on one of two machines, A and B, and machine A was used more often in the earlier times while B was used more often later. If the tissue samples belong to two groups control (C) and treatment (T) samples were not equally run on one of two machines, this approach may has bias. We should rescale the data base on the information of machines.

## (c) Design and run a small simulation experiment to demonstrate the superiority of your idea.


```r
set.seed(1)
AC = matrix(rnorm(10*1000,mean = 1),ncol = 1000)
AT = matrix(rnorm(40*1000,mean = 2),ncol = 1000)
BC = matrix(rnorm(40*1000,mean = 4),ncol = 1000)
BT = matrix(rnorm(10*1000,mean = 5),ncol = 1000)
dim(AC)
```

```
## [1]   10 1000
```

```r
data6=rbind(AC,AT,BC,BT)
dim(data6)
```

```
## [1]  100 1000
```

```r
groupindex6=c(rep("AC", 10), rep("AT", 40), rep("BC", 40), rep("BT", 10))
pca.out6 <- prcomp(data6)
summary(pca.out6)
```

```
## Importance of components:
##                            PC1     PC2     PC3     PC4     PC5     PC6
## Standard deviation     40.3599 4.12159 4.10136 4.08409 4.05613 4.02379
## Proportion of Variance  0.6203 0.00647 0.00641 0.00635 0.00627 0.00617
## Cumulative Proportion   0.6203 0.62678 0.63319 0.63954 0.64580 0.65197
##                            PC7     PC8     PC9    PC10    PC11    PC12
## Standard deviation     3.98548 3.95797 3.92792 3.90080 3.84668 3.82340
## Proportion of Variance 0.00605 0.00597 0.00588 0.00579 0.00563 0.00557
## Cumulative Proportion  0.65802 0.66399 0.66986 0.67565 0.68129 0.68686
##                          PC13    PC14   PC15    PC16    PC17    PC18
## Standard deviation     3.8012 3.79400 3.7654 3.76256 3.72032 3.70920
## Proportion of Variance 0.0055 0.00548 0.0054 0.00539 0.00527 0.00524
## Cumulative Proportion  0.6924 0.69784 0.7032 0.70863 0.71390 0.71914
##                           PC19    PC20    PC21    PC22    PC23    PC24
## Standard deviation     3.69315 3.65317 3.64842 3.61091 3.60526 3.56414
## Proportion of Variance 0.00519 0.00508 0.00507 0.00497 0.00495 0.00484
## Cumulative Proportion  0.72434 0.72942 0.73449 0.73945 0.74440 0.74924
##                           PC25    PC26    PC27    PC28    PC29    PC30
## Standard deviation     3.55326 3.53053 3.51951 3.50728 3.49473 3.46969
## Proportion of Variance 0.00481 0.00475 0.00472 0.00468 0.00465 0.00458
## Cumulative Proportion  0.75405 0.75879 0.76351 0.76820 0.77285 0.77743
##                           PC31    PC32    PC33   PC34    PC35    PC36
## Standard deviation     3.45779 3.44844 3.41795 3.3980 3.38378 3.36794
## Proportion of Variance 0.00455 0.00453 0.00445 0.0044 0.00436 0.00432
## Cumulative Proportion  0.78198 0.78651 0.79096 0.7954 0.79972 0.80404
##                           PC37    PC38    PC39    PC40    PC41    PC42
## Standard deviation     3.35484 3.34495 3.33698 3.30067 3.28761 3.27384
## Proportion of Variance 0.00429 0.00426 0.00424 0.00415 0.00412 0.00408
## Cumulative Proportion  0.80832 0.81258 0.81683 0.82097 0.82509 0.82917
##                           PC43    PC44   PC45    PC46    PC47    PC48
## Standard deviation     3.26667 3.25179 3.2424 3.23183 3.20895 3.19381
## Proportion of Variance 0.00406 0.00403 0.0040 0.00398 0.00392 0.00388
## Cumulative Proportion  0.83324 0.83726 0.8413 0.84524 0.84916 0.85305
##                           PC49    PC50    PC51    PC52    PC53    PC54
## Standard deviation     3.16277 3.15657 3.14684 3.13189 3.10469 3.09279
## Proportion of Variance 0.00381 0.00379 0.00377 0.00374 0.00367 0.00364
## Cumulative Proportion  0.85686 0.86065 0.86442 0.86816 0.87183 0.87547
##                           PC55    PC56   PC57    PC58    PC59    PC60
## Standard deviation     3.05482 3.04528 3.0302 3.00991 3.00163 2.99119
## Proportion of Variance 0.00355 0.00353 0.0035 0.00345 0.00343 0.00341
## Cumulative Proportion  0.87903 0.88256 0.8861 0.88950 0.89293 0.89634
##                           PC61    PC62    PC63    PC64    PC65   PC66
## Standard deviation     2.97439 2.95489 2.91424 2.91129 2.90631 2.9000
## Proportion of Variance 0.00337 0.00333 0.00323 0.00323 0.00322 0.0032
## Cumulative Proportion  0.89971 0.90304 0.90627 0.90950 0.91271 0.9159
##                           PC67    PC68    PC69    PC70    PC71    PC72
## Standard deviation     2.87778 2.85944 2.83624 2.83499 2.79632 2.78413
## Proportion of Variance 0.00315 0.00311 0.00306 0.00306 0.00298 0.00295
## Cumulative Proportion  0.91907 0.92218 0.92525 0.92831 0.93129 0.93424
##                           PC73    PC74    PC75   PC76    PC77    PC78
## Standard deviation     2.78242 2.76834 2.74283 2.7096 2.69689 2.69249
## Proportion of Variance 0.00295 0.00292 0.00286 0.0028 0.00277 0.00276
## Cumulative Proportion  0.93719 0.94010 0.94297 0.9458 0.94854 0.95130
##                           PC79    PC80    PC81   PC82    PC83    PC84
## Standard deviation     2.67303 2.65397 2.65049 2.6117 2.60286 2.56653
## Proportion of Variance 0.00272 0.00268 0.00268 0.0026 0.00258 0.00251
## Cumulative Proportion  0.95402 0.95670 0.95937 0.9620 0.96455 0.96706
##                           PC85    PC86    PC87    PC88    PC89   PC90
## Standard deviation     2.55246 2.53968 2.52128 2.49204 2.47302 2.4601
## Proportion of Variance 0.00248 0.00246 0.00242 0.00236 0.00233 0.0023
## Cumulative Proportion  0.96954 0.97200 0.97442 0.97678 0.97911 0.9814
##                           PC91    PC92    PC93    PC94    PC95    PC96
## Standard deviation     2.43258 2.40968 2.39598 2.33938 2.32495 2.32060
## Proportion of Variance 0.00225 0.00221 0.00219 0.00208 0.00206 0.00205
## Cumulative Proportion  0.98367 0.98588 0.98807 0.99015 0.99221 0.99426
##                           PC97    PC98    PC99    PC100
## Standard deviation     2.28486 2.23688 2.20115 5.01e-15
## Proportion of Variance 0.00199 0.00191 0.00185 0.00e+00
## Cumulative Proportion  0.99625 0.99815 1.00000 1.00e+00
```

```r
biplot(pca.out6,scale = 0,cex=0.5)

names(pca.out6)
```

```
## [1] "sdev"     "rotation" "center"   "scale"    "x"
```

```r
scorematrix6=as.data.frame(pca.out6$x)
dim(scorematrix6)
```

```
## [1] 100 100
```

```r
library(ggplot2)
```

![](chapter10-1_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
ggplot(scorematrix6, aes(x=PC1, y=PC2, color=groupindex6)) + geom_point()
```

![](chapter10-1_files/figure-html/unnamed-chunk-2-2.png)<!-- -->

```r
newAC= AC/1.5
newAT= AT/1.5
newBC= BC/4.5
newBT= BT/4.5

dim(newAC)
```

```
## [1]   10 1000
```

```r
data6new=rbind(newAC,newAT,newBC,newBT)
dim(data6new)
```

```
## [1]  100 1000
```

```r
pca.out6new <- prcomp(data6new)
summary(pca.out6new)
```

```
## Importance of components:
##                           PC1    PC2     PC3     PC4     PC5     PC6
## Standard deviation     7.8911 2.5541 2.53069 2.51995 2.46424 2.43946
## Proportion of Variance 0.2023 0.0212 0.02081 0.02063 0.01973 0.01934
## Cumulative Proportion  0.2023 0.2235 0.24434 0.26498 0.28471 0.30405
##                            PC7     PC8     PC9    PC10    PC11   PC12
## Standard deviation     2.43897 2.40674 2.39714 2.37264 2.35896 2.3535
## Proportion of Variance 0.01933 0.01882 0.01867 0.01829 0.01808 0.0180
## Cumulative Proportion  0.32338 0.34220 0.36087 0.37916 0.39724 0.4152
##                           PC13    PC14    PC15    PC16    PC17    PC18
## Standard deviation     2.32183 2.29953 2.27452 2.26394 2.24805 2.23816
## Proportion of Variance 0.01752 0.01718 0.01681 0.01665 0.01642 0.01628
## Cumulative Proportion  0.43276 0.44994 0.46675 0.48341 0.49983 0.51610
##                          PC19    PC20    PC21    PC22    PC23    PC24
## Standard deviation     2.2327 2.22276 2.20302 2.17929 2.16010 2.13935
## Proportion of Variance 0.0162 0.01605 0.01577 0.01543 0.01516 0.01487
## Cumulative Proportion  0.5323 0.54836 0.56413 0.57956 0.59472 0.60959
##                           PC25    PC26    PC27    PC28    PC29   PC30
## Standard deviation     2.12601 2.12287 2.10601 2.09414 2.08542 2.0755
## Proportion of Variance 0.01469 0.01464 0.01441 0.01425 0.01413 0.0140
## Cumulative Proportion  0.62428 0.63892 0.65334 0.66759 0.68172 0.6957
##                           PC31    PC32    PC33    PC34    PC35    PC36
## Standard deviation     2.05408 2.03984 2.00585 2.00242 1.97152 1.95748
## Proportion of Variance 0.01371 0.01352 0.01307 0.01303 0.01263 0.01245
## Cumulative Proportion  0.70942 0.72294 0.73602 0.74905 0.76168 0.77413
##                           PC37    PC38    PC39    PC40    PC41    PC42
## Standard deviation     1.94999 1.92650 1.91295 1.88314 1.86911 1.85864
## Proportion of Variance 0.01236 0.01206 0.01189 0.01152 0.01135 0.01123
## Cumulative Proportion  0.78648 0.79854 0.81043 0.82196 0.83331 0.84453
##                          PC43    PC44   PC45    PC46    PC47   PC48
## Standard deviation     1.8395 1.82080 1.7976 1.76769 1.75150 1.7275
## Proportion of Variance 0.0110 0.01077 0.0105 0.01015 0.00997 0.0097
## Cumulative Proportion  0.8555 0.86630 0.8768 0.88696 0.89692 0.9066
##                           PC49    PC50    PC51    PC52    PC53   PC54
## Standard deviation     1.65831 1.62213 0.87109 0.84983 0.83667 0.8235
## Proportion of Variance 0.00894 0.00855 0.00247 0.00235 0.00227 0.0022
## Cumulative Proportion  0.91556 0.92411 0.92657 0.92892 0.93119 0.9334
##                           PC55    PC56    PC57    PC58    PC59    PC60
## Standard deviation     0.79229 0.78600 0.78286 0.78033 0.77133 0.76579
## Proportion of Variance 0.00204 0.00201 0.00199 0.00198 0.00193 0.00191
## Cumulative Proportion  0.93544 0.93744 0.93944 0.94141 0.94335 0.94525
##                           PC61    PC62    PC63    PC64    PC65    PC66
## Standard deviation     0.75291 0.74978 0.74779 0.74177 0.73591 0.73061
## Proportion of Variance 0.00184 0.00183 0.00182 0.00179 0.00176 0.00173
## Cumulative Proportion  0.94710 0.94892 0.95074 0.95253 0.95429 0.95602
##                          PC67    PC68    PC69    PC70    PC71    PC72
## Standard deviation     0.7230 0.72116 0.71029 0.70761 0.69384 0.69174
## Proportion of Variance 0.0017 0.00169 0.00164 0.00163 0.00156 0.00155
## Cumulative Proportion  0.9577 0.95941 0.96105 0.96268 0.96424 0.96580
##                           PC73    PC74    PC75    PC76    PC77    PC78
## Standard deviation     0.69095 0.68350 0.68094 0.67581 0.67459 0.66849
## Proportion of Variance 0.00155 0.00152 0.00151 0.00148 0.00148 0.00145
## Cumulative Proportion  0.96735 0.96886 0.97037 0.97186 0.97333 0.97479
##                           PC79    PC80    PC81    PC82    PC83    PC84
## Standard deviation     0.66562 0.66088 0.65380 0.64709 0.63969 0.63558
## Proportion of Variance 0.00144 0.00142 0.00139 0.00136 0.00133 0.00131
## Cumulative Proportion  0.97623 0.97765 0.97903 0.98039 0.98172 0.98304
##                           PC85    PC86    PC87    PC88    PC89   PC90
## Standard deviation     0.62886 0.62665 0.62222 0.61503 0.61168 0.6082
## Proportion of Variance 0.00129 0.00128 0.00126 0.00123 0.00122 0.0012
## Cumulative Proportion  0.98432 0.98560 0.98686 0.98809 0.98930 0.9905
##                           PC91    PC92   PC93    PC94    PC95    PC96
## Standard deviation     0.60406 0.59225 0.5815 0.57584 0.57035 0.56315
## Proportion of Variance 0.00119 0.00114 0.0011 0.00108 0.00106 0.00103
## Cumulative Proportion  0.99169 0.99283 0.9939 0.99500 0.99606 0.99709
##                           PC97    PC98    PC99     PC100
## Standard deviation     0.55678 0.55034 0.53107 3.504e-15
## Proportion of Variance 0.00101 0.00098 0.00092 0.000e+00
## Cumulative Proportion  0.99810 0.99908 1.00000 1.000e+00
```

```r
biplot(pca.out6new,scale = 0,cex=0.5)
```

![](chapter10-1_files/figure-html/unnamed-chunk-2-3.png)<!-- -->

```r
names(pca.out6new)
```

```
## [1] "sdev"     "rotation" "center"   "scale"    "x"
```

```r
scorematrix6new=as.data.frame(pca.out6new$x)
dim(scorematrix6new)
```

```
## [1] 100 100
```

```r
ggplot(scorematrix6new, aes(x=PC1, y=PC2, color=groupindex6)) + geom_point()
```

![](chapter10-1_files/figure-html/unnamed-chunk-2-4.png)<!-- -->


# 8. In Section 10.2.3, a formula for calculating PVE was given in Equation 10.8. We also saw that the PVE can be obtained using the sdev output of the prcomp() function.
On the USArrests data, calculate PVE in two ways:

## (a) Using the sdev output of the prcomp() function, as was done in Section 10.2.3.


```r
pr.out=prcomp(USArrests, scale=TRUE)
pr.out
```

```
## Standard deviations (1, .., p=4):
## [1] 1.5748783 0.9948694 0.5971291 0.4164494
## 
## Rotation (n x k) = (4 x 4):
##                 PC1        PC2        PC3         PC4
## Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
## Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
## UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
## Rape     -0.5434321 -0.1673186  0.8177779  0.08902432
```

```r
pr.out$sdev
```

```
## [1] 1.5748783 0.9948694 0.5971291 0.4164494
```

```r
pr.var=pr.out$sdev^2
pr.var
```

```
## [1] 2.4802416 0.9897652 0.3565632 0.1734301
```

```r
pve=pr.var/sum(pr.var)
pve
```

```
## [1] 0.62006039 0.24744129 0.08914080 0.04335752
```


## (b) By applying Equation 10.8 directly. That is, use the prcomp() function to compute the principal component loadings. Then, use those loadings in Equation 10.8 to obtain the PVE. 
These two approaches should give the same results.
Hint: You will only obtain the same results in (a) and (b) if the same data is used in both cases. For instance, if in (a) you performed prcomp() using centered and scaled variables, then you must center and scale the variables before applying Equation 10.3 in (b).


```r
pr.out=prcomp(USArrests, scale=TRUE)
pr.out
```

```
## Standard deviations (1, .., p=4):
## [1] 1.5748783 0.9948694 0.5971291 0.4164494
## 
## Rotation (n x k) = (4 x 4):
##                 PC1        PC2        PC3         PC4
## Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
## Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
## UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
## Rape     -0.5434321 -0.1673186  0.8177779  0.08902432
```

```r
newUSArrests=scale(USArrests)
summary(USArrests)
```

```
##      Murder          Assault         UrbanPop          Rape      
##  Min.   : 0.800   Min.   : 45.0   Min.   :32.00   Min.   : 7.30  
##  1st Qu.: 4.075   1st Qu.:109.0   1st Qu.:54.50   1st Qu.:15.07  
##  Median : 7.250   Median :159.0   Median :66.00   Median :20.10  
##  Mean   : 7.788   Mean   :170.8   Mean   :65.54   Mean   :21.23  
##  3rd Qu.:11.250   3rd Qu.:249.0   3rd Qu.:77.75   3rd Qu.:26.18  
##  Max.   :17.400   Max.   :337.0   Max.   :91.00   Max.   :46.00
```

```r
summary(newUSArrests)
```

```
##      Murder           Assault           UrbanPop             Rape        
##  Min.   :-1.6044   Min.   :-1.5090   Min.   :-2.31714   Min.   :-1.4874  
##  1st Qu.:-0.8525   1st Qu.:-0.7411   1st Qu.:-0.76271   1st Qu.:-0.6574  
##  Median :-0.1235   Median :-0.1411   Median : 0.03178   Median :-0.1209  
##  Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.00000   Mean   : 0.0000  
##  3rd Qu.: 0.7949   3rd Qu.: 0.9388   3rd Qu.: 0.84354   3rd Qu.: 0.5277  
##  Max.   : 2.2069   Max.   : 1.9948   Max.   : 1.75892   Max.   : 2.6444
```

```r
head(newUSArrests)
```

```
##                Murder   Assault   UrbanPop         Rape
## Alabama    1.24256408 0.7828393 -0.5209066 -0.003416473
## Alaska     0.50786248 1.1068225 -1.2117642  2.484202941
## Arizona    0.07163341 1.4788032  0.9989801  1.042878388
## Arkansas   0.23234938 0.2308680 -1.0735927 -0.184916602
## California 0.27826823 1.2628144  1.7589234  2.067820292
## Colorado   0.02571456 0.3988593  0.8608085  1.864967207
```

```r
score=pr.out$rotation
score
```

```
##                 PC1        PC2        PC3         PC4
## Murder   -0.5358995  0.4181809 -0.3412327  0.64922780
## Assault  -0.5831836  0.1879856 -0.2681484 -0.74340748
## UrbanPop -0.2781909 -0.8728062 -0.3780158  0.13387773
## Rape     -0.5434321 -0.1673186  0.8177779  0.08902432
```

```r
newUSArrests.score=newUSArrests%*%score
head(newUSArrests.score)
```

```
##                   PC1        PC2         PC3          PC4
## Alabama    -0.9756604  1.1220012 -0.43980366  0.154696581
## Alaska     -1.9305379  1.0624269  2.01950027 -0.434175454
## Arizona    -1.7454429 -0.7384595  0.05423025 -0.826264240
## Arkansas    0.1399989  1.1085423  0.11342217 -0.180973554
## California -2.4986128 -1.5274267  0.59254100 -0.338559240
## Colorado   -1.4993407 -0.9776297  1.08400162  0.001450164
```

```r
e10.8up=apply(newUSArrests.score^2,2,sum)
e10.8low=sum(apply(newUSArrests^2,2,sum))
e10.8up/e10.8low
```

```
##        PC1        PC2        PC3        PC4 
## 0.62006039 0.24744129 0.08914080 0.04335752
```


# 10. In this problem, you will generate simulated data, and then perform PCA and K-means clustering on the data.

## (a) Generate a simulated data set with 20 observations in each of three classes (i.e. 60 observations total), and 50 variables.
Hint: There are a number of functions in R that you can use to generate data. One example is the rnorm() function; runif() is another option. Be sure to add a mean shift to the observations in each class so that there are three distinct classes.


```r
set.seed(1)
A = matrix(rnorm(1000,mean = 1),ncol = 50)
B = matrix(rnorm(1000,mean = 2),ncol = 50)
C = matrix(rnorm(1000,mean = 3),ncol = 50)
dim(C)
```

```
## [1] 20 50
```

```r
data10=rbind(A,B,C)
dim(data10)
```

```
## [1] 60 50
```


## (b) Perform PCA on the 60 observations and plot the ﬁrst two principal component score vectors. Use a diﬀerent color to indicate the observations in each of the three classes. If the three classes appear separated in this plot, then continue on to part (c). If not, then return to part (a) and modify the simulation so that there is greater separation between the three classes. Do not continue to part (c) until the three classes show at least some separation in the ﬁrst two principal component score vectors.


```r
groupindex=c(rep("A", 20), rep("B", 20), rep("C", 20))
pca.out <- prcomp(data10)
biplot(pca.out,scale = 0,cex=0.5)
```

![](chapter10-1_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
names(pca.out)
```

```
## [1] "sdev"     "rotation" "center"   "scale"    "x"
```

```r
scorematrix=as.data.frame(pca.out$x)
colnames(scorematrix)
```

```
##  [1] "PC1"  "PC2"  "PC3"  "PC4"  "PC5"  "PC6"  "PC7"  "PC8"  "PC9"  "PC10"
## [11] "PC11" "PC12" "PC13" "PC14" "PC15" "PC16" "PC17" "PC18" "PC19" "PC20"
## [21] "PC21" "PC22" "PC23" "PC24" "PC25" "PC26" "PC27" "PC28" "PC29" "PC30"
## [31] "PC31" "PC32" "PC33" "PC34" "PC35" "PC36" "PC37" "PC38" "PC39" "PC40"
## [41] "PC41" "PC42" "PC43" "PC44" "PC45" "PC46" "PC47" "PC48" "PC49" "PC50"
```

```r
library(ggplot2)
ggplot(scorematrix, aes(x=PC1, y=PC2, color=groupindex)) + geom_point()
```

![](chapter10-1_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

