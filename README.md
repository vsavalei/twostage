
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Render `README.Rmd` regularly to keep `README.md` up-to-date, via
`devtools::build_readme()` -->

<<<<<<< HEAD
# Package `twostage`

## Introduction
=======
# twostage
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f

<!-- badges: start -->

[![R-CMD-check](https://github.com/vsavalei/twostage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vsavalei/twostage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<<<<<<< HEAD
The goal of the `twostage` package is to provide a convenient way to fit
composite-level structural equation models (SEMs) to item-level data,
using several different methods. The most common applications are: 1)
SEMs with parcels and 2) path analysis with scale scores. The main
purpose of the methods included is to handle missing data at the item
level. When data are complete, all methods will return identical (or
highly similar, depending on information matrix settings) results
compared to the approach where composites are computed directly, and a
model is fit to them. The composites are always sums or averages of the
items; the weights are all equal and are fixed rather than estimated.
=======
The goal of twostage is to provide a convenient way to fit
composite-level structural equation models (SEMs) to item-level data,
using several different methods. The most common applications are: 1)
SEMs with parcels and 2) path analysis models with scale scores. The
main purpose of the methods included is to handle missing data at the
item level. When data are complete, all methods will return identical
(or highly similar, depending on information matrix settings) results
compared to the approach where composites are computed directly, and a
model is fit to them. The composites are always sums or averages of the
items, rather than having estimated weights.
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f

The package includes the following methods:

1)  Two-stage ML (TSML) method of Savalei and Rhemtulla (2017a)
2)  PIM (Pseudo-Indicator Model) method of Rose, Wagner, Mayer, and
    Nagengast (2019)
3)  (yet to be implemented) the GLS method of Savalei and Rhemtulla
    (2017b)

<<<<<<< HEAD
In the future, the package may also include  
confirmatory composite analysis (CCA; Henseler, YEAR), which is similar
to PIM in that it models composites as latent variables, but it does so
in a non-saturated way: i.e., the results will be different than fitting
a model directly to composites even with complete data. This method
forms composites by finding optimal weights that optimize its
relationships with other constructs. CCA is currently implemented in the
development version of `lavaan`, but not in a way that would allow
missing data on the indicators of exogenous formative variables, so its
inclusion in this package would allow for a comparison of performance
with missing data.
=======
In the future, the package may also include the method called
confirmatory composite analysis (CCA; Henseler, YEAR), which includes
items and composites (as formative constructs) in the model in a
non-saturated way: i.e., the results will be different than fitting a
model directly to composites even with complete data. This method forms
composites using weights that optimize its relationships with other
constructs, and it allows to test the hypothesis that relationships
between the formative construct and other variables in the model
(whether formative or reflective constructs, or observed variables)
flows only through the formative construct, and not the individual
items. This method is currently being implemented in the development
version of `lavaan`, but not in a way that would allow missing data on
the indicators of exogenous formative variables, so its inclusion in
this package may be needed.
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f

## Installation

You can install the development version of twostage from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("vsavalei/twostage")
```

<<<<<<< HEAD
You can load it in the usual way:
=======
## Example

This is a basic example using a built-in simulated dataset
`misdata_mcar20`, which contains 27 items, $Y_1$ to $Y_27$ (where about
half have 20% missing data). The model is for composites $C_1$ to $C_9$,
which are parcels formed by adding three items each, in order; for
example, $C_1 = Y_1 + Y_2 + Y_3$, and so on. These composites are
actually never implicitly computed under any of these approaches. The
composite model is a 3-factor model, with three indicators each, defined
via `lavaan` syntax as follows:
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f

``` r
library(twostage) 
#> This package is written by a newbie. You've been warned.
```

<<<<<<< HEAD
## Example

This is a basic example using a built-in simulated dataset
`misdata_mcar20`, which contains 27 items, $Y_1$ to $Y_{27}$, where
about half have 20% missing data. The model is for composites $C_1$ to
$C_9$, which are parcels formed by adding three items each, in order;
for example, $C_1 = Y_1 + Y_2 + Y_3$, and so on. However, these
composites are never implicitly computed under any of the three
methods.  
The composite model is a 3-factor model, with three indicators each,
defined via `lavaan` syntax as follows:

=======
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f
``` r
#composite model
mod <- '
F1 =~ C1 + C2 + C3
F2 =~ C4 + C5 + C6
F3 =~ C7 + C8 + C9
'
```

To fit this model, we require the specification of a $27 \times 9$
matrix $C$, whose columns are labeled with component names: $Y_1$ to
<<<<<<< HEAD
$Y_{27}$, and whose rows are labeled with composite names: $C_1$ to
$C_9$. The \[i,j\]th element of $C$ is nonzero (for sums, it is 1) if
component $j$ belongs to composite $i$, and zero otherwise. To easily
create this matrix using an interactive interface, use:
=======
$Y_27$, and whose columns are labeled with composite names: $C_1$ to
$C_9$. The \[i,j\]th element of $C$ contains a nonzero value (usually 1,
for sums) if component $j$ belongs to composite $i$ and zero otherwise.
To easily create this matrix using an interactive interface, use:
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f

    C <- stage0(data=misdata_mcar20,model=mod)

This function will first ask you whether your composites are sums or
averages, and then it will ask you to assign each component to one of
<<<<<<< HEAD
the composites that appear in the model. \[ADD/ADD FEATURE: If you
select None, that variable will appear as itself in the model. See the
vignette on treating observed covariates that are not composites\]
=======
the composites that appear in the model. \[ADD: If you select None, that
variable will appear as itself in the model. See the vignette on
treating observed covariates that are not composites\]
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f

You will get the following message to confirm your assignment:

    Your composites are made up of the following components: 
    C1 :  Y1 Y2 Y3 
    C2 :  Y4 Y5 Y6 
    C3 :  Y7 Y8 Y9 
    C4 :  Y10 Y11 Y12 
    C5 :  Y13 Y14 Y15 
    C6 :  Y16 Y17 Y18 
    C7 :  Y19 Y20 Y21 
    C8 :  Y22 Y23 Y24 
    C9 :  Y25 Y26 Y27 
    If this is not correct, start over! 

<<<<<<< HEAD
Once this matrix is created with the help of the `stage0` function or
manually, the composite-level model can be fit via item-level two-stage
(also known as TSML), like so:

    #> [1] "Two-stage parameter estimates, naive standard errors, and two-stage standard errors:"
    #>    lhs op rhs         est        se s2$TS_SEs
    #> 2   F1 =~  C2  1.02193658 0.3579328 0.3790369
    #> 3   F1 =~  C3  1.38926575 0.4766224 0.5078380
    #> 5   F2 =~  C5  1.19819027 0.3712583 0.4204921
    #> 6   F2 =~  C6  1.29248625 0.3870511 0.4327614
    #> 8   F3 =~  C8  0.50252629 0.2247838 0.2413742
    #> 9   F3 =~  C9  1.21419672 0.3532458 0.4010741
    #> 10  C1 ~~  C1  3.01850878 0.3907862 0.4168094
    #> 11  C2 ~~  C2  3.68118084 0.4557431 0.4951376
    #> 12  C3 ~~  C3  2.78671087 0.5204495 0.5704592
    #> 13  C4 ~~  C4  3.57287005 0.4145500 0.4718739
    #> 14  C5 ~~  C5  3.45781977 0.4373025 0.5011573
    #> 15  C6 ~~  C6  2.91885671 0.4145512 0.4623456
    #> 16  C7 ~~  C7  2.74306637 0.3628939 0.4131604
    #> 17  C8 ~~  C8  3.16669743 0.3332076 0.3679581
    #> 18  C9 ~~  C9  3.13697082 0.4649928 0.5285742
    #> 19  F1 ~~  F1  0.67578595 0.3270536 0.3441623
    #> 20  F2 ~~  F2  0.64868483 0.3078294 0.3455100
    #> 21  F3 ~~  F3  0.76015959 0.3223813 0.3671408
    #> 22  F1 ~~  F2  0.34270935 0.1495455 0.1614227
    #> 23  F1 ~~  F3  0.33202639 0.1508528 0.1661643
    #> 24  F2 ~~  F3  0.58960029 0.2008429 0.2222176
    #> 25  C1 ~1      0.25996869 0.1359098 0.1410413
    #> 26  C2 ~1     -0.11576178 0.1481037 0.1520786
    #> 27  C3 ~1      0.11284636 0.1430213 0.1482134
    #> 28  C4 ~1      0.08381759 0.1452851 0.1549407
    #> 29  C5 ~1      0.05747750 0.1481403 0.1584999
    #> 30  C6 ~1     -0.02888770 0.1414655 0.1516308
    #> 31  C7 ~1     -0.11409123 0.1323485 0.1431886
    #> 32  C8 ~1      0.11175214 0.1295890 0.1400749
    #> 33  C9 ~1      0.03200253 0.1459050 0.1537131
    #> [1] "The residual based chi-square is 24.827 against 24 degrees of freedom, with a p-value of 0.415"
=======
Once the assignment of components to composites is clear via this
matrix, the model can be fit via item-level two-stage (also known as
TSML), like so:

``` r
out_ts <- twostage(data = misdata_mcar20, model = mod, C = C)
#> [1] "Two-stage parameter estimates, naive standard errors, and two-stage standard errors:"
#>    lhs op rhs         est        se s2$TS_SEs
#> 2   F1 =~  C2  1.02193658 0.3579328 0.3790369
#> 3   F1 =~  C3  1.38926575 0.4766224 0.5078380
#> 5   F2 =~  C5  1.19819027 0.3712583 0.4204921
#> 6   F2 =~  C6  1.29248625 0.3870511 0.4327614
#> 8   F3 =~  C8  0.50252629 0.2247838 0.2413742
#> 9   F3 =~  C9  1.21419672 0.3532458 0.4010741
#> 10  C1 ~~  C1  3.01850878 0.3907862 0.4168094
#> 11  C2 ~~  C2  3.68118084 0.4557431 0.4951376
#> 12  C3 ~~  C3  2.78671087 0.5204495 0.5704592
#> 13  C4 ~~  C4  3.57287005 0.4145500 0.4718739
#> 14  C5 ~~  C5  3.45781977 0.4373025 0.5011573
#> 15  C6 ~~  C6  2.91885671 0.4145512 0.4623456
#> 16  C7 ~~  C7  2.74306637 0.3628939 0.4131604
#> 17  C8 ~~  C8  3.16669743 0.3332076 0.3679581
#> 18  C9 ~~  C9  3.13697082 0.4649928 0.5285742
#> 19  F1 ~~  F1  0.67578595 0.3270536 0.3441623
#> 20  F2 ~~  F2  0.64868483 0.3078294 0.3455100
#> 21  F3 ~~  F3  0.76015959 0.3223813 0.3671408
#> 22  F1 ~~  F2  0.34270935 0.1495455 0.1614227
#> 23  F1 ~~  F3  0.33202639 0.1508528 0.1661643
#> 24  F2 ~~  F3  0.58960029 0.2008429 0.2222176
#> 25  C1 ~1      0.25996869 0.1359098 0.1410413
#> 26  C2 ~1     -0.11576178 0.1481037 0.1520786
#> 27  C3 ~1      0.11284636 0.1430213 0.1482134
#> 28  C4 ~1      0.08381759 0.1452851 0.1549407
#> 29  C5 ~1      0.05747750 0.1481403 0.1584999
#> 30  C6 ~1     -0.02888770 0.1414655 0.1516308
#> 31  C7 ~1     -0.11409123 0.1323485 0.1431886
#> 32  C8 ~1      0.11175214 0.1295890 0.1400749
#> 33  C9 ~1      0.03200253 0.1459050 0.1537131
#> [1] "The residual based chi-square is 24.827 against 24 degrees of freedom, with a p-value of 0.415"
```
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f

\[Describe output once `summary(out_ts)` is available\]

To fit the PIM model, we first create the PIM syntax, as follows:

``` r
modpim <- PIM_syntax(compmodel = mod, C = C)
```

The resulting syntax is long and can be viewed via `cat(modpim)`. It
<<<<<<< HEAD
contains the definition of each composite $C_i$, $i=1,\ldots,9$, as a
single-indicator latent variable, and then a special structure on the
items.

We then fit the PIM model using FIML in `lavaan`, as follows:

    #> lavaan 0.6-20.2265 ended normally after 261 iterations
    #> 
    #>   Estimator                                         ML
    #>   Optimization method                           NLMINB
    #>   Number of model parameters                       381
    #> 
    #>   Number of observations                           200
    #>   Number of missing patterns                        36
    #> 
    #> Model Test User Model:
    #>                                                       
    #>   Test statistic                                24.311
    #>   Degrees of freedom                                24
    #>   P-value (Chi-square)                           0.444
=======
contains the definition of each composite $C$ as a single-indicator
latent variable, and then a special structure on the items to ensure
each $C$ is just a unit-weighted composite, and that the relationship
between the items and other variables in the model is saturated.

We then fit the PIM model using FIML in `lavaan`, as follows:

``` r
fitpim <- lavaan::sem(modpim, data=misdata_mcar20,missing="FIML")
fitpim
#> lavaan 0.6-20.2265 ended normally after 261 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                       381
#> 
#>   Number of observations                           200
#>   Number of missing patterns                        36
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                24.311
#>   Degrees of freedom                                24
#>   P-value (Chi-square)                           0.444
```

Are the factors correlated??
>>>>>>> 782d012664d0700b7c01bd981ee16aef66ad351f

## To-do list (where to put this?)

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

7)  Rename into something reflecting items to composites? item2stage?
    items2comps?

8)  Need to include None as an option to stage0 function? and a lot of
    useful warning messages

9)  Need an average model for PIM (currently sums only). Maybe create
    arbitrary fixed weights for all (longer range?)

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

1)  add the two-stage saturated method to lavaan (make it easy for Yves)
    and extend current CCA implementation of lavaan so that constraints
    can be relaxed, yielding PIM

2)  Research question: comparison to SAM (Yves added support for
    second-level CFA)
