---
output: html_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->

# twostage

<!-- badges: start -->
<!-- badges: end -->

The goal of twostage is to two-stage!

## Installation

You can install the development version of twostage from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("vsavalei/twostage")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(twostage)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks.

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this.

You can also embed plots, for example:

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## How to handle observed variables that are not composites

There are two options:

1)  Assign these variables to their own composites, so that
    single-indicator latent variables are created for them. The
    advantage of this option is that the generated PIM syntax will be
    correct right away. The disadvantage is that a different name (from
    the observed variable’s name) has to be assigned to this
    “composite”, making the interpretation of the model results a little
    awkward.

2)  Include these variables in the model directly as observed variables.
    The advantage of this option is that the model is more
    interpretable. The disadvantage is that the generated PIM syntax
    will have to be modified manually to correlate observed and latent
    exogenous predictors. This is quite likely since true composites are
    treated as latent variables in PIM.

To do so, rename this variable in the composite model, and either
regenerate C using stage0 function or rename the corresponding row of C.
However, this may require manual modification of the resulting PIM
syntax to correlate observed and latent exogeneous predictors (see
Github issue \#415.

Both approaches are illustrated below.

``` r
##PIM syntax tpbmod example

tpbmod<-'
INTALL ~ ATTALL + PBCALL + NORMALL
BEH ~ INTALL'


#made up example with a single-indicator composite
#C1 is the sum of Y1, Y2, and Y3
#' #C2 is the sum of Y4, Y5, and Y6
#' #C3 is Y7
C<-matrix(0,nrow=3,ncol=7)
C[1,1:3]<-1
C[2,4:6]<-1
C[3,7]<-1
rownames(C)<-c("C1","C2","C3")
colnames(C)<-c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")
compmodel<-"C1 ~ C2 + C3"
model1 <- PIM_syntax(C,compmodel)
#> Note: The composite named C3 has only one component: Y7. 
#> Keep this in mind when interpreting the results! See README.md for alternatives.
cat(model1)
#> 
#> C1 =~ 1*Y1
#>   Y1 ~ (-1)*Y2 + (-1)*Y3
#>    Y1 ~~ 0*Y1 
#>  C1 ~ 1  
#>  Y1 ~ 0*1
#> C2 =~ 1*Y4
#>   Y4 ~ (-1)*Y5 + (-1)*Y6
#>    Y4 ~~ 0*Y4 
#>  C2 ~ 1  
#>  Y4 ~ 0*1
#> C3 =~ 1*Y7
#>   Y7 ~~ 0*Y7 
#>  C3 ~ 1  
#>  Y7 ~ 0*1
#> Y2 ~~ Y3
#> Y2 ~~ Y5
#> Y2 ~~ Y6
#> Y3 ~~ Y5
#> Y3 ~~ Y6
#> Y5 ~~ Y6 
#> C1 ~~ Y2+Y3+Y5+Y6 
#>  C2 ~~ Y2+Y3+Y5+Y6 
#>  C3 ~~ Y2+Y3+Y5+Y6
#> C1 ~ C2 + C3
data1 <- misdata_mcar20[,c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")]
out <- lavaan::sem(model1,data=data1)
df <- as.numeric(fitmeasures(out)["df"])
df
#> [1] 0

#made up example renaming C3 into Y7:
rownames(C)<-c("C1","C2","Y7")
compmodel<-"C1 ~ C2 + Y7"
model2 <- PIM_syntax(C,compmodel)
#> Note: The following variables are treated as observed variables: Y7 
#> The generated PIM syntax may need to be manually modified to allow 
#> their correlation with other exogeneous variables that are composites, 
#> which are set up as latent variables in PIM. See README.md for more detail.
cat(model2)
#> 
#> C1 =~ 1*Y1
#>   Y1 ~ (-1)*Y2 + (-1)*Y3
#>    Y1 ~~ 0*Y1 
#>  C1 ~ 1  
#>  Y1 ~ 0*1
#> C2 =~ 1*Y4
#>   Y4 ~ (-1)*Y5 + (-1)*Y6
#>    Y4 ~~ 0*Y4 
#>  C2 ~ 1  
#>  Y4 ~ 0*1
#> Y2 ~~ Y3
#> Y2 ~~ Y5
#> Y2 ~~ Y6
#> Y3 ~~ Y5
#> Y3 ~~ Y6
#> Y5 ~~ Y6 
#> C1 ~~ Y2+Y3+Y5+Y6 
#>  C2 ~~ Y2+Y3+Y5+Y6 
#>  Y7 ~~ Y2+Y3+Y5+Y6
#> C1 ~ C2 + Y7
out2 <- lavaan::sem(model2,data=data1) #,orthogonal.x=FALSE) #,fixed.x=FALSE,missing="ML")
df2 <- as.numeric(fitmeasures(out2)["df"])
df2
#> [1] 1
lavInspect(out2,"cov.all") #0 for Y7 and C2
#>        Y1     Y4     Y2     Y3     Y5     Y6     Y7     C1     C2
#> Y1  1.041                                                        
#> Y4  0.086  1.174                                                 
#> Y2  0.140  0.042  0.988                                          
#> Y3  0.025  0.067  0.174  1.186                                   
#> Y5  0.178  0.064  0.075  0.005  0.955                            
#> Y6  0.110  0.039  0.195  0.055  0.368  1.114                     
#> Y7 -0.100 -0.154  0.077  0.160  0.085  0.070  0.862              
#> C1  1.206  0.196  1.302  1.385  0.257  0.360  0.137  3.894       
#> C2  0.374  1.276  0.311  0.127  1.387  1.521  0.000  0.813  4.184


model2.mod<-paste(model2,"\n Y7 ~~C2") #Y7~~Y7 doesn't work
out2.mod <- lavaan::sem(model2.mod,data=data1)
out2.mod
#> lavaan 0.6-19 ended normally after 63 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        35
#> 
#>                                                   Used       Total
#>   Number of observations                           160         200
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#hmm, so why does the default to correlate predictors go away
#when one of the predictors is observed?
```
