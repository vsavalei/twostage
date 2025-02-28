
<!-- README.md is generated from README.Rmd. Please edit that file -->

# note to self

Render `README.Rmd` regularly to keep `README.md` up-to-date, via
`devtools::build_readme()`

# twostage

<!-- badges: start -->
<!-- badges: end -->

The goal of twostage (which may be renamed into item2stage to be more
accurate) is to provide a convenient way to fit composite-level models
to item-level data. For now, this package focuses on methods that do
this in a saturated way; that is, the hypothesis that relationships
between a formative construct and other variables in the model (whether
formative or reflective constructs, or observed variables) flows only
through the formative composite is not tested. Furthermore, there is
currently now way to adapt these methods to include such tests. That
means that these methods are primarily for handling missing data at the
item level, when the model is at the composite level; they serve no
other purpose and will return identical results to a model that is just
fit to the composites instead of the raw items when the data are
complete.

The package currently includes the following methods: 1) Two-stage ML
(TSML) method of Savalei and Rhemtulla (2017a) 2) PIM (Pseudo-Indicator
Model) method of Rose, Wagner, Mayer, and Nagengast (2019) 3) (yet to be
implemented) the GLS method of Savalei and Rhemtulla (2017b).

## To-do list

Soon:

1)  Provide `show()` and `summary()` for `twostage()` function only (not
    for component functions), that looks much like lavaan output, but
    with ML replaced with TSML, test statistic and p-value replaced with
    res test statistic and p-value, and standard errors replaced with
    two-stage standard errors (and labeled that way)

2)  Add GLS

3)  Reconsider how documentation is done for TSML functions – maybe
    join, use inherit

4)  Add robust TSML (Actually, I think this will just work if we pass
    `MLR` to lavaan, but check, write a vignette)

5)  Reconsider if constructing C is the best way to store info on
    composites vs components (though it is needed for computations in
    TSML and GLS, but not in PM). E.g., maybe it should be a list.

6)  Add tests for all functions (currently, only one test is included,
    to pass the rmd check)

Longer range?:

1)  Provide accurate fit indices for model fit assessments that assess
    the composite-level model only. For the PIM, this may have to be
    nested fit indices, e.g., $\text{RMSEA}_\text{D}$. How to do fit
    assessment for these models is probably at least one paper. The fit
    assessment is a limited of the default PIM implementation, as is
    noted in the original article.

2)  Implement methods that do test hypotheses about relationships
    flowing only through composites, i.e., confirmatory composite
    analysis (CCA), e.g., Schuberth, Henseler, and Ogasawara (2021),
    Henseler (book, year?). Currently, Yves has provided a preliminary
    implementation of this approach in lavaan, but it has limitations,
    such as it cannot handle missing data on items that correspond to
    exogenous formative latent variables! It also restricts some
    covariances among items so that constraints imposed by CCA cannot be
    relaxed to obtain a PIM (these are nested models; see Siegel,
    Savalei, and Rhemtulla (2025; submitted to PM).

3)  Provide a way for the user to specify how components relate to
    composites using newly invented lavaan syntax (developed for CCA)
    for this. Example of such syntax:

`model <- '   eta1 <~ x1 + x2   eta2 <~ x3 + x4 + x5'`

This should be translatable to a 5 x 2 matrix C, with rows labeled etas
and columns labeled xs, and with 1s corresponding to those xs that
belong to certain etas. We could look into how lavaan parses this syntax
and stores the result.

Longer longer longer term goal:

1)  add the two-stage saturated method to lavaan (make it easy for Yves
    to accept this) and extend current CCA implementation of lavaan so
    that constraints can be relaxed, yielding PIM

## Installation

You can install the development version of twostage from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("vsavalei/twostage")
```

## Example

This is a basic example:

``` r
library(twostage)
#> This package is written by a newbie. You've been warned. Please send comments!
## basic example code
```

\[To add later, for now see examples\]

## Move to a vignette: How to handle observed variables that are not composites

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
syntax to correlate observed and latent exogenous predictors (see Github
issue \#415.

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
df <- as.numeric(lavaan::fitmeasures(out)["df"])
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
df2 <- as.numeric(lavaan::fitmeasures(out2)["df"])
df2
#> [1] 1
lavaan::lavInspect(out2,"cov.all") #0 for Y7 and C2
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
#> lavaan 0.6-20.2265 ended normally after 63 iterations
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
#when one of the predictors is observed? this is now reported on 
#github, at Yves' request so that he doesn't forget: #414
#as is, we have to explain to the user that this is happening and PIM
#syntax will be incomplete 
```
