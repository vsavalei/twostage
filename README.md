
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Render `README.Rmd` regularly to keep `README.md` up-to-date, via
`devtools::build_readme()` -->

# Package `twostage`

## Introduction

<!-- badges: start -->

[![R-CMD-check](https://github.com/vsavalei/twostage/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/vsavalei/twostage/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of the `twostage` package is to provide helpful automation to
fit composite-level structural equation models (SEMs) to item-level data
using the SEM R package `lavaan`. The most common applications are: 1)
SEMs with parcels and 2) path analysis with scale scores.

The package includes automation for the following approaches:

1)  The two-stage ML (TSML) method of Savalei and Rhemtulla (2017a)
2)  The Pseudo-Indicator Model (PIM) of Rose, Wagner, Mayer, and
    Nagengast (2019)
3)  (yet to be implemented) the GLS method of Savalei and Rhemtulla
    (2017b)

The main purpose of these three approaches is to handle missing data at
the item level. When data are complete, these methods/models will return
identical (or highly similar, depending on information matrix settings)
results compared to the approach where composites are computed directly,
and a model is fit to them (see vignette TBA). The composites are always
sums or averages of the items; the weights are all equal and are fixed
rather than estimated.

In the future, the package may also include confirmatory composite
analysis (CCA; Henseler, YEAR), which is similar to PIM in that it
models composites as latent variables, but it 1) finds optimally
weighted composites (for prediction), and 2) does not allow indicators
to influence other variables directly (but only through the composite).
This approach will *not* return identical results to composite models
with complete data, and is thus qualitatively different. CCA is
currently being implemented in the development version of `lavaan`, but
not in a way that would allow missing data on the indicators of
exogenous composites. Including it in `twostage` would enable a study of
its performance with missing data.

## Installation

You can install the development version of twostage from
[GitHub](https://github.com/) with:

``` r
install.packages("pak")
pak::pak("vsavalei/twostage")
```

You can load it in the usual way:

``` r
library(twostage) 
#> This package is written by a newbie. You've been warned.
```

<!-- too large? hard to illustrate PIM -->

## Example

This example uses a built-in simulated dataset `misdata_mcar20`, which
contains 27 items, $Y_1$ to $Y_{27}$, where about half have 20% missing
data. The model is for composites $C_1$ to $C_9$, which are parcels of
three items each, in order; for example, $C_1 = Y_1 + Y_2 + Y_3$, and so
on. These composites are never explicitly computed under any of the
three methods.

The composite model is a 3-factor model, with three indicators each,
defined via `lavaan` syntax as follows:

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
$Y_{27}$, and whose rows are labeled with composite names: $C_1$ to
$C_9$. The \[i,j\]th element of $C$ is nonzero (for sums, it is 1) if
component $j$ belongs to composite $i$, and zero otherwise. To easily
create this matrix using an interactive interface, use:

    C <- stage0(data=misdata_mcar20,model=mod)

This function will first ask you whether your composites are sums or
averages, and then it will ask you to assign each component to one of
the composites that appear in the model. \[ADD/ADD FEATURE: If you
select None, that variable will appear as itself in the model. See the
vignette on treating observed covariates that are not composites\]

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

Once this matrix is created with the help of the `stage0` function or
manually, the composite-level model can be fit using the methods
included in the package.

To fit it using it TSML:

``` r
out_ts <- twostage(data = misdata_mcar20, model = mod, C = C)
summary(out_ts)
#> Summary of Two-Stage Analysis 
#> ----------------------------
#> Two-stage parameter estimates, naive standard errors, and two-stage standard errors: 
#>  lhs op rhs         est        se   TSML se
#>   F1 =~  C2  1.02193658 0.3579328 0.3790369
#>   F1 =~  C3  1.38926575 0.4766224 0.5078380
#>   F2 =~  C5  1.19819027 0.3712583 0.4204921
#>   F2 =~  C6  1.29248625 0.3870511 0.4327614
#>   F3 =~  C8  0.50252629 0.2247838 0.2413742
#>   F3 =~  C9  1.21419672 0.3532458 0.4010741
#>   C1 ~~  C1  3.01850878 0.3907862 0.4168094
#>   C2 ~~  C2  3.68118084 0.4557431 0.4951376
#>   C3 ~~  C3  2.78671087 0.5204495 0.5704592
#>   C4 ~~  C4  3.57287005 0.4145500 0.4718739
#>   C5 ~~  C5  3.45781977 0.4373025 0.5011573
#>   C6 ~~  C6  2.91885671 0.4145512 0.4623456
#>   C7 ~~  C7  2.74306637 0.3628939 0.4131604
#>   C8 ~~  C8  3.16669743 0.3332076 0.3679581
#>   C9 ~~  C9  3.13697082 0.4649928 0.5285742
#>   F1 ~~  F1  0.67578595 0.3270536 0.3441623
#>   F2 ~~  F2  0.64868483 0.3078294 0.3455100
#>   F3 ~~  F3  0.76015959 0.3223813 0.3671408
#>   F1 ~~  F2  0.34270935 0.1495455 0.1614227
#>   F1 ~~  F3  0.33202639 0.1508528 0.1661643
#>   F2 ~~  F3  0.58960029 0.2008429 0.2222176
#>   C1 ~1      0.25996869 0.1359098 0.1410413
#>   C2 ~1     -0.11576178 0.1481037 0.1520786
#>   C3 ~1      0.11284636 0.1430213 0.1482134
#>   C4 ~1      0.08381759 0.1452851 0.1549407
#>   C5 ~1      0.05747750 0.1481403 0.1584999
#>   C6 ~1     -0.02888770 0.1414655 0.1516308
#>   C7 ~1     -0.11409123 0.1323485 0.1431886
#>   C8 ~1      0.11175214 0.1295890 0.1400749
#>   C9 ~1      0.03200253 0.1459050 0.1537131
#> ----------------------------
#> The residual-based TSML chi-square is 24.827 against 24 degrees of freedom, with a p-value of 0.415
```

The output shows TSML parameter estimates from Stage 2, “naive” standard
errors, and TSML standard errors, which are generally larger, reflecting
greater uncertainty due to missing data in Stage 1. The residual-based
test statistic is also printed. This output assumes normality. For
technical details on the standard error and test statistic computation,
see \[Savalei and Bentler, 2009\]
(<https://www.tandfonline.com/doi/full/10.1080/10705510903008238>)

To fit it within a larger PIM model, we first create the `lavaan` PIM
syntax, as follows:

``` r
modpim <- PIM_syntax(compmodel = mod, C = C)
```

The resulting syntax is long and can be viewed via `cat(modpim)`. It
contains the definition of each composite $C_i$, $i=1,\ldots,9$, as a
single-indicator latent variable, and a special structure on the items.

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
ests <- lavaan::parameterestimates(fitpim)

ests_comp <- ests[!grepl("Y", ests[, "lhs"]) & !grepl("Y", ests[, "rhs"]), ]
ests_comp
#>     lhs op rhs    est    se      z pvalue ci.lower ci.upper
#> 5    C1 ~1      0.263 0.141  1.866  0.062   -0.013    0.538
#> 11   C2 ~1     -0.120 0.152 -0.789  0.430   -0.417    0.178
#> 17   C3 ~1      0.115 0.148  0.774  0.439   -0.176    0.405
#> 23   C4 ~1      0.079 0.155  0.507  0.612   -0.225    0.383
#> 29   C5 ~1      0.049 0.158  0.307  0.759   -0.261    0.358
#> 35   C6 ~1     -0.031 0.152 -0.203  0.839   -0.328    0.267
#> 41   C7 ~1     -0.119 0.144 -0.823  0.410   -0.402    0.164
#> 47   C8 ~1      0.110 0.140  0.784  0.433   -0.165    0.385
#> 53   C9 ~1      0.038 0.153  0.248  0.804   -0.263    0.339
#> 370  F1 =~  C1  1.000 0.000     NA     NA    1.000    1.000
#> 371  F1 =~  C2  1.089 0.415  2.626  0.009    0.276    1.901
#> 372  F1 =~  C3  1.386 0.543  2.554  0.011    0.322    2.449
#> 373  F2 =~  C4  1.000 0.000     NA     NA    1.000    1.000
#> 374  F2 =~  C5  1.248 0.440  2.838  0.005    0.386    2.111
#> 375  F2 =~  C6  1.269 0.422  3.008  0.003    0.442    2.096
#> 376  F3 =~  C7  1.000 0.000     NA     NA    1.000    1.000
#> 377  F3 =~  C8  0.511 0.242  2.113  0.035    0.037    0.985
#> 378  F3 =~  C9  1.173 0.414  2.831  0.005    0.361    1.986
#> 397  C1 ~~  C1  3.028 0.428  7.069  0.000    2.189    3.868
#> 398  C2 ~~  C2  3.590 0.505  7.106  0.000    2.600    4.580
#> 399  C3 ~~  C3  2.833 0.574  4.934  0.000    1.708    3.958
#> 400  C4 ~~  C4  3.620 0.473  7.658  0.000    2.693    4.546
#> 401  C5 ~~  C5  3.393 0.497  6.828  0.000    2.419    4.367
#> 402  C6 ~~  C6  2.997 0.471  6.366  0.000    2.074    3.920
#> 403  C7 ~~  C7  2.791 0.438  6.375  0.000    1.933    3.650
#> 404  C8 ~~  C8  3.171 0.375  8.457  0.000    2.436    3.906
#> 405  C9 ~~  C9  3.189 0.512  6.227  0.000    2.185    4.192
#> 406  F1 ~~  F1  0.665 0.355  1.876  0.061   -0.030    1.360
#> 407  F2 ~~  F2  0.642 0.342  1.881  0.060   -0.027    1.312
#> 408  F3 ~~  F3  0.779 0.393  1.984  0.047    0.010    1.548
#> 409  F1 ~~  F2  0.339 0.163  2.074  0.038    0.019    0.659
#> 410  F1 ~~  F3  0.312 0.171  1.824  0.068   -0.023    0.647
#> 411  F2 ~~  F3  0.606 0.231  2.621  0.009    0.153    1.060
#> 430  F1 ~1      0.000 0.000     NA     NA    0.000    0.000
#> 431  F2 ~1      0.000 0.000     NA     NA    0.000    0.000
#> 432  F3 ~1      0.000 0.000     NA     NA    0.000    0.000
```

This is a standard *lavaan* model; the user just has to ignore a lot of
extraneous output pertaining to the item parameters. In the  
above, the output of the `parameterestimates` function has been truncated by
removing all rows referring to a “Y” variable (i.e., an item).

The only complicating element for TS and PIM methods is approximate fit
assessment; this aspect is under development, more on this is in
[Approximate_fit vignette](../doc/Approximate_fit.html).

When data are complete, both TS and PIM produce equivalent output to the
complete data run on the manually-formed composites, as shown in
[Complete_data vignette](../doc/Complete_data.html).
