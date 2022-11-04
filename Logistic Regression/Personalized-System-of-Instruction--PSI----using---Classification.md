Personalized System of Instruction (PSI) - using - Classification
================

``` r
knitr::opts_chunk$set(echo = TRUE)
```

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
library(readxl)
library(margins)
library(ROCR)
```

    ## 
    ## Attaching package: 'ROCR'

    ## The following object is masked from 'package:margins':
    ## 
    ##     prediction

# 2.0 Read data

``` r
grade =read_excel("C:/personal files/data analytics/docs/MODULE 3/t1/Grade.xlsx")
```

# 3.0 Inspecting Data

``` r
dim(grade)
```

    ## [1] 32  5

``` r
str(grade)
```

    ## tibble [32 x 5] (S3: tbl_df/tbl/data.frame)
    ##  $ OBS  : num [1:32] 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ GPA  : num [1:32] 2.66 2.89 3.28 2.92 4 2.86 2.76 2.87 3.03 3.92 ...
    ##  $ TUCE : num [1:32] 20 22 24 12 21 17 17 21 25 29 ...
    ##  $ PSI  : num [1:32] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ GRADE: num [1:32] 0 0 0 0 1 0 0 0 0 1 ...

``` r
summary(grade)
```

    ##       OBS             GPA             TUCE            PSI        
    ##  Min.   : 1.00   Min.   :2.060   Min.   :12.00   Min.   :0.0000  
    ##  1st Qu.: 8.75   1st Qu.:2.812   1st Qu.:19.75   1st Qu.:0.0000  
    ##  Median :16.50   Median :3.065   Median :22.50   Median :0.0000  
    ##  Mean   :16.50   Mean   :3.117   Mean   :21.94   Mean   :0.4375  
    ##  3rd Qu.:24.25   3rd Qu.:3.515   3rd Qu.:25.00   3rd Qu.:1.0000  
    ##  Max.   :32.00   Max.   :4.000   Max.   :29.00   Max.   :1.0000  
    ##      GRADE       
    ##  Min.   :0.0000  
    ##  1st Qu.:0.0000  
    ##  Median :0.0000  
    ##  Mean   :0.3438  
    ##  3rd Qu.:1.0000  
    ##  Max.   :1.0000

``` r
boxplot(grade)
```

![](Personalized-System-of-Instruction--PSI----using---Classification_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

-   The dataset constitutes 5 columns and 32 rows.
-   All the vaiables are in numeric format.
-   OBS and TUCE has higher variance and medians as compared GPA . PSI ,
    GRADE.

## 3.1 Getting glimpse of data

``` r
head(grade)
```

    ## # A tibble: 6 x 5
    ##     OBS   GPA  TUCE   PSI GRADE
    ##   <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1     1  2.66    20     0     0
    ## 2     2  2.89    22     0     0
    ## 3     3  3.28    24     0     0
    ## 4     4  2.92    12     0     0
    ## 5     5  4       21     0     1
    ## 6     6  2.86    17     0     0

``` r
tail(grade)
```

    ## # A tibble: 6 x 5
    ##     OBS   GPA  TUCE   PSI GRADE
    ##   <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1    27  3.39    17     1     1
    ## 2    28  2.67    24     1     0
    ## 3    29  3.65    21     1     1
    ## 4    30  4       23     1     1
    ## 5    31  3.1     21     1     0
    ## 6    32  2.39    19     1     1

# 4.0 Modelling

## 4.1 LPM

``` r
LPM=lm(GRADE ~ GPA + TUCE + PSI , grade)
summary(LPM)
```

    ## 
    ## Call:
    ## lm(formula = GRADE ~ GPA + TUCE + PSI, data = grade)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.78153 -0.27731  0.00531  0.21089  0.81145 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept) -1.49802    0.52389  -2.859  0.00793 **
    ## GPA          0.46385    0.16196   2.864  0.00784 **
    ## TUCE         0.01050    0.01948   0.539  0.59436   
    ## PSI          0.37855    0.13917   2.720  0.01109 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.3881 on 28 degrees of freedom
    ## Multiple R-squared:  0.4159, Adjusted R-squared:  0.3533 
    ## F-statistic: 6.646 on 3 and 28 DF,  p-value: 0.001571

-   As seen from linear probablity model the Rsq looses its significance
    in binary distribution of the predictors
-   Let’s run the logit model to overcome this issue

## 4.2 Logistic Regression (Logit Function)

``` r
logit_model =glm(GRADE ~ GPA + TUCE + PSI, data = grade , family  = binomial("logit"))
summary(logit_model)
```

    ## 
    ## Call:
    ## glm(formula = GRADE ~ GPA + TUCE + PSI, family = binomial("logit"), 
    ##     data = grade)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9551  -0.6453  -0.2570   0.5888   2.0966  
    ## 
    ## Coefficients:
    ##              Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept) -13.02135    4.93127  -2.641  0.00828 **
    ## GPA           2.82611    1.26293   2.238  0.02524 * 
    ## TUCE          0.09516    0.14155   0.672  0.50143   
    ## PSI           2.37869    1.06456   2.234  0.02545 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 41.183  on 31  degrees of freedom
    ## Residual deviance: 25.779  on 28  degrees of freedom
    ## AIC: 33.779
    ## 
    ## Number of Fisher Scoring iterations: 5

-   We can see that only GPA and PSI turns out to be significant having
    significance level .05.

##### Calculating Margins

``` r
margins(logit_model)
```

    ## Average marginal effects

    ## glm(formula = GRADE ~ GPA + TUCE + PSI, family = binomial("logit"),     data = grade)

    ##     GPA    TUCE    PSI
    ##  0.3626 0.01221 0.3052

#### Proportions of dependent variable

``` r
proportions(table(grade$GRADE))
```

    ## 
    ##       0       1 
    ## 0.65625 0.34375

-   The Distribution of grades is slightly skewed.

##### Plotting Actual vs Fitted results

``` r
plot(logit_model$fitted.values ,type = "l", ylim= c(0,1),col = "blue" , lwd = 2, lty = 1 , main  = "FITTED vs ACTUAL (Logit)", xlab = "Index" , ylab="Probablity")
lines(grade$GRADE , type = "p" , col= "red" , pch =20)
logit_model$fitted.values
```

    ##          1          2          3          4          5          6          7 
    ## 0.02657799 0.05950126 0.18725993 0.02590164 0.56989295 0.03485827 0.02650406 
    ##          8          9         10         11         12         13         14 
    ## 0.05155900 0.11112666 0.69351131 0.02447037 0.18999744 0.32223955 0.19321116 
    ##         15         16         17         18         19         20         21 
    ## 0.36098992 0.03018375 0.05362641 0.03858834 0.58987249 0.66078584 0.06137585 
    ##         22         23         24         25         26         27         28 
    ## 0.90484727 0.24177245 0.85209089 0.83829051 0.48113304 0.63542059 0.30721866 
    ##         29         30         31         32 
    ## 0.84170413 0.94534025 0.52911720 0.11103084

``` r
legend("center",c("Predicted" , "Actual") , fill = c("blue" , "red"))
```

![](Personalized-System-of-Instruction--PSI----using---Classification_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

-   Seeing the Line chart we can confirm that there are more proportion
    of Fail “0” than Pass “1” and hance there is slight bump in
    probability at index of 22 that is and constitutes about 30% of
    grades.

## 4.2 Logistic Regression (probit)

``` r
probit_model =glm(GRADE ~  GPA + TUCE + PSI, family = binomial("probit"), 
    data = grade)

summary(probit_model)
```

    ## 
    ## Call:
    ## glm(formula = GRADE ~ GPA + TUCE + PSI, family = binomial("probit"), 
    ##     data = grade)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -1.9392  -0.6508  -0.2229   0.5934   2.0451  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept) -7.45231    2.57152  -2.898  0.00376 **
    ## GPA          1.62581    0.68973   2.357  0.01841 * 
    ## TUCE         0.05173    0.08119   0.637  0.52406   
    ## PSI          1.42633    0.58695   2.430  0.01510 * 
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 41.183  on 31  degrees of freedom
    ## Residual deviance: 25.638  on 28  degrees of freedom
    ## AIC: 33.638
    ## 
    ## Number of Fisher Scoring iterations: 6

-   We can see that only GPA turns out to be significant having
    significance level .05.

##### Calculating margins

``` r
margins(probit_model)
```

    ## Average marginal effects

    ## glm(formula = GRADE ~ GPA + TUCE + PSI, family = binomial("probit"),     data = grade)

    ##     GPA    TUCE    PSI
    ##  0.3608 0.01148 0.3165

### Plotting Actual vs Fitted results

``` r
plot(probit_model$fitted.values ,type = "l", ylim= c(0,1),col = "green" , lwd = 4, lty = 1 , main  = "FITTED vs ACTUAL (Probit)", xlab = "Index" , ylab="Probablity")
lines(grade$GRADE , type = "p" , col= "yellow" , pch =10)

legend("center",c("Predicted" , "Actual") , fill = c("green" , "yellow"))
```

![](Personalized-System-of-Instruction--PSI----using---Classification_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

-   Seeing the Line chart we can confirm that there are more proportion
    of Fail “0” than Pass “1” and hance there is slight bump in
    probability at index of 22 that is and constitutes about 30% of
    grades.

##### 4.3 Thresholding

``` r
alpha= seq(0,1 , by=.1)

acc_logit= c()
acc_probit= c()
acc_lpm=c()

for (a1 in 1:length(alpha)){
  logit.fitted=ifelse(logit_model$fitted.values >alpha[a1] ,1 ,0)
  misclassifier1 = mean(logit.fitted != grade$GRADE)
  acc_logit[a1]=1- misclassifier1
}


for (a1 in 1:length(alpha)){
 
  probit.fitted=ifelse(probit_model$fitted.values >alpha[a1] ,1 ,0)
  misclassifier2 = mean(probit.fitted != grade$GRADE)
  acc_probit[a1]=1- misclassifier2
}

for (a1 in 1:length(alpha)){
 
  lpm.fitted=ifelse(LPM$fitted.values >alpha[a1] ,1 ,0)
  misclassifier3 = mean(lpm.fitted != grade$GRADE)
  acc_lpm[a1]= 1-misclassifier3
}





x2=data.frame(threshold=alpha, logit_model.accuracy= acc_logit ,  probit_model.accuracy=acc_probit ,lpm_model.accuracy=acc_lpm)

x2 %>% filter(logit_model.accuracy == max(x2$logit_model.accuracy))
```

    ##   threshold logit_model.accuracy probit_model.accuracy lpm_model.accuracy
    ## 1       0.4              0.84375               0.84375            0.81250
    ## 2       0.6              0.84375               0.84375            0.84375

``` r
ggplot(x2 , aes(x=threshold ))+geom_line(aes(y=logit_model.accuracy), color = "red",lwd= 2)+ geom_line(aes(y=probit_model.accuracy), color= "blue",lwd= 2)+ geom_line(aes(y=lpm_model.accuracy), color = "green" , lwd= 2)+ labs(title= "Threshold vs Accuracy"  )+ xlab("Threshold")+ ylab("Accuracy")+theme_minimal()
```

![](Personalized-System-of-Instruction--PSI----using---Classification_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

-   The accuracy line for logit and probit is roughly similar while LPM
    model gives better accuracy only at few threshold values.
-   Looking at overall accuracy, the logit and probit model has more
    accuratcy in most of the thresholds especially when threshold is in
    between than .1 to .5 with highest accuracy of .81% at threshold of
    0.4 and 0.5 .

##### 3.4 Receiver Operator Curve-

``` r
plot(performance(ROCR::prediction(logit_model$fitted.values, grade$GRADE), measure = "tpr", x= "fpr"), main="ROC Curve" , col="salmon" ,print.auc=TRUE, lwd= 3)

plot(performance(ROCR::prediction(probit_model$fitted.values, grade$GRADE), measure = "tpr", x= "fpr") , add =TRUE , col ="goldenrod" ,print.auc=TRUE , lwd= 3)

plot(performance(ROCR::prediction(LPM$fitted.values, grade$GRADE), measure = "tpr", x= "fpr"), add =TRUE , col = "lightsteelblue" ,print.auc=TRUE, lwd= 3)

legend("bottom",
       legend=c("LOGIT", "PROBIT", "LPM"),
       col=c("salmon", "goldenrod", "lightsteelblue"),
       lwd=4, cex =0.4, xpd = TRUE, horiz = TRUE)

abline(0, 1, lwd=2, lty=1)
```

![](Personalized-System-of-Instruction--PSI----using---Classification_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
## Area under curve
auc1= performance(ROCR::prediction(logit_model$fitted.values, grade$GRADE), measure= "auc")
auc2= performance(ROCR::prediction(probit_model$fitted.values, grade$GRADE), measure= "auc")
auc3= performance(ROCR::prediction(LPM$fitted.values, grade$GRADE), measure= "auc")
paste("auc for logit ,model" ,auc1@y.values[[1]])
```

    ## [1] "auc for logit ,model 0.883116883116883"

``` r
paste("auc for probit model" ,auc2@y.values[[1]])
```

    ## [1] "auc for probit model 0.887445887445887"

``` r
paste ("auc for lpm model" ,auc3@y.values[[1]])
```

    ## [1] "auc for lpm model 0.891774891774892"

-   Logit and Probit model shows same Roc curve with higher AUC than LPM
    . Hence we can say that Logit and Probit model are the better fit to
    the model.
