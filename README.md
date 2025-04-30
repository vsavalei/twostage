
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Render `README.Rmd` regularly tokeep `README.md` up-to-date, via `devtools::build_readme()` -->
<!-- To view better, run: 
detach("package:twostage", unload = TRUE)
pkgdown::build_site() 
-->

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

1)  The two-stage ML (TSML) method of [Savalei and Rhemtulla
    (2017a)](https://journals.sagepub.com/doi/full/10.3102/1076998617694880)
2)  The Pseudo-Indicator Model (PIM) of [Rose, Wagner, Mayer, and
    Nagengast
    (2019)](https://online.ucpress.edu/collabra/article/5/1/9/112958/Model-Based-Manifest-and-Latent-Composite-Scores)
3)  (yet to be added) the GLS method of [Savalei and Rhemtulla
    (2017b)](https://www.frontiersin.org/journals/psychology/articles/10.3389/fpsyg.2017.00767/full)

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

This example uses a simulated dataset `misdata_mcar20`, which contains
27 items, $Y_1$ to $Y_{27}$, where about half have 20% missing data. The
model is for composites $C_1$ to $C_9$, which are parcels of three items
each, in order; for example, $C_1 = Y_1 + Y_2 + Y_3$, and so on. These
composites are never explicitly computed under any of the three methods.

The composite model is a 3-factor model, with three indicators each,
defined via `lavaan` syntax as follows:

``` r
#composite model
mod <- '
F1 =~ 1*C1 + C2 + C3
F2 =~ 1*C4 + C5 + C6
F3 =~ 1*C7 + C8 + C9
'
```

To fit this composite model using the item-level methods in this
package, the specification of a $27 \times 9$ matrix $C$ assigning
components to composites is first required. The columns are labeled with
component names: $Y_1$ to $Y_{27}$, and the rows are labeled with
composite names: $C_1$ to $C_9$. The \[i,j\]th element of $C$ is nonzero
(for sums, it is 1) if component $j$ belongs to composite $i$, and zero
otherwise. To create this matrix using an interactive interface, use:

    C <- stage0(data=misdata_mcar20,model=mod)

This function will first ask the user whether the composites are sums or
averages, and then it will ask the user to assign each component to one
of the variable names (assumed to be composites) that appear in the
model. The following message will confirm the assignment:

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

Once the $C$ matrix is created with the help of the `stage0` function
(or manually), the composite-level model can be fit using the methods
included in the package.

To fit the composite-level model using TSML:

``` r
fit_ts <- twostage(data = misdata_mcar20, model = mod, C = C)
summary(fit_ts)
#> Summary of Two-Stage Analysis 
#> ----------------------------
#> Parameter estimates from Stage 2, naive standard errors from lavaan (with z-test and p-value),
#>  and the corrected TSML standard errors (with z-test and p-value): 
#>  lhs op rhs    est se_naive z_naive pvalue_naive    se      z pvalue
#>   F1 =~  C2  1.022    0.358   2.855        0.004 0.379  2.696  0.007
#>   F1 =~  C3  1.389    0.477   2.915        0.004 0.508  2.736  0.006
#>   F2 =~  C5  1.198    0.371   3.227        0.001 0.420  2.849  0.004
#>   F2 =~  C6  1.292    0.387   3.339        0.001 0.433  2.987  0.003
#>   F3 =~  C8  0.503    0.225   2.236        0.025 0.241  2.082  0.037
#>   F3 =~  C9  1.214    0.353   3.437        0.001 0.401  3.027  0.002
#>   C1 ~~  C1  3.019    0.391   7.724        0.000 0.417  7.242  0.000
#>   C2 ~~  C2  3.681    0.456   8.077        0.000 0.495  7.435  0.000
#>   C3 ~~  C3  2.787    0.520   5.354        0.000 0.570  4.885  0.000
#>   C4 ~~  C4  3.573    0.415   8.619        0.000 0.472  7.572  0.000
#>   C5 ~~  C5  3.458    0.437   7.907        0.000 0.501  6.900  0.000
#>   C6 ~~  C6  2.919    0.415   7.041        0.000 0.462  6.313  0.000
#>   C7 ~~  C7  2.743    0.363   7.559        0.000 0.413  6.639  0.000
#>   C8 ~~  C8  3.167    0.333   9.504        0.000 0.368  8.606  0.000
#>   C9 ~~  C9  3.137    0.465   6.746        0.000 0.529  5.935  0.000
#>   F1 ~~  F1  0.676    0.327   2.066        0.039 0.344  1.964  0.050
#>   F2 ~~  F2  0.649    0.308   2.107        0.035 0.346  1.877  0.060
#>   F3 ~~  F3  0.760    0.322   2.358        0.018 0.367  2.070  0.038
#>   F1 ~~  F2  0.343    0.150   2.292        0.022 0.161  2.123  0.034
#>   F1 ~~  F3  0.332    0.151   2.201        0.028 0.166  1.998  0.046
#>   F2 ~~  F3  0.590    0.201   2.936        0.003 0.222  2.653  0.008
#>   C1 ~1      0.260    0.136   1.913        0.056 0.141  1.843  0.065
#>   C2 ~1     -0.116    0.148  -0.782        0.434 0.152 -0.761  0.447
#>   C3 ~1      0.113    0.143   0.789        0.430 0.148  0.761  0.446
#>   C4 ~1      0.084    0.145   0.577        0.564 0.155  0.541  0.589
#>   C5 ~1      0.057    0.148   0.388        0.698 0.158  0.363  0.717
#>   C6 ~1     -0.029    0.141  -0.204        0.838 0.152 -0.191  0.849
#>   C7 ~1     -0.114    0.132  -0.862        0.389 0.143 -0.797  0.426
#>   C8 ~1      0.112    0.130   0.862        0.388 0.140  0.798  0.425
#>   C9 ~1      0.032    0.146   0.219        0.826 0.154  0.208  0.835
#> ----------------------------
#> The residual-based TSML chi-square is 24.952 against 24 degrees of freedom, with a p-value of 0.408
```

The output shows TSML parameter estimates from Stage 2, “naive” standard
errors, and TSML standard errors, which are generally larger, reflecting
greater uncertainty due to missing data in Stage 1. The residual-based
test statistic is also printed. This output assumes normality. For
technical details on the standard errors and the residual-based test
statistic computation, see [Savalei and Bentler (
2009)](https://www.tandfonline.com/doi/full/10.1080/10705510903008238)
and [Savalei and Rhemtulla
(2017a)](https://journals.sagepub.com/doi/full/10.3102/1076998617694880).

To fit the composite model using PIM, the package automates the creation
of the `lavaan` PIM syntax:

``` r
modpim <- PIM_syntax(compmodel = mod, C = C)
#> Note: The following exogeneous variables in the composites model are correlated: F1, F2, F3. 
#> If you do not want this, modify the composite model syntax manually or set exog_vars=FALSE.
```

The resulting syntax is long and can be viewed via `cat(modpim)`. It
contains the definition of each composite $C_i$, $i=1,\ldots,9$, as a
single-indicator latent variable, and a special structure on the items.
For details, see [Rose, Wagner, Mayer, and Nagengast
(2019).](https://online.ucpress.edu/collabra/article/5/1/9/112958/Model-Based-Manifest-and-Latent-Composite-Scores)
It is recommend to always check the part of the generated syntax
pertaining to the composites (at the bottom). The default in
`PIM_syntax` is to covary exogenous variables (whether observed
variables, latent variables, or composites); to undo this default, use
`exog_cov=FALSE`.

The PIM model can be fit directly in `lavaan`. To treat missing data on
the items, use FIML:

``` r
fit_pim <- lavaan::lavaan(modpim, data=misdata_mcar20,missing="FIML")
#> Warning: lavaan->lav_object_post_check():  
#>    covariance matrix of latent variables is not positive definite ; use 
#>    lavInspect(fit, "cov.lv") to investigate.
fit_pim
#> lavaan 0.6-20.2307 ended normally after 296 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                       378
#> 
#>   Number of observations                           200
#>   Number of missing patterns                        36
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                72.898
#>   Degrees of freedom                                27
#>   P-value (Chi-square)                           0.000
```

Always confirm that the degrees of freedom are what you would expect for
your composite model (if composites were formed directly). In this
example,`fit_pm` returns the same df as `fit_ts`: 24. While the `sem` or
`cfa` function produces identical output in this case, it is best to use
the `lavaan()` function to fit PIMs to avoid any unintended defaults,
given the nonstandard setup of this model. A complication is when there
are true latent variables in the model, as in this example (F1 to F3),
the `lavaan` function does not automatically fix their metric via the
marker variable approach. This is the reason `modpim` has explicit 1s
added to identify the marker variables. Alternatively, you could have
fixed variances of F1-F3 to 1 in the model syntax. In the future we will
automate this step.

To inspect parameter estimates, use `summary(fit_pim)` or the
`parameterEstimates` function of `lavaan`. As the interest is in the
composite model, the user has to ignore a lot of extraneous output
pertaining to the item parameters. In the output shown below, we have
removed all rows referring to “Y” variables (i.e., items).

``` r
ests <- lavaan::parameterEstimates(fit_pim,remove.nonfree=TRUE)
ests_comp <- ests[!grepl("Y", ests[, "lhs"]) & !grepl("Y", ests[, "rhs"]), ]
ests_comp
#>     lhs op rhs    est    se      z pvalue ci.lower ci.upper
#> 5    C1 ~1      0.258 0.141  1.833  0.067   -0.018    0.533
#> 7    C1 ~~  C1  3.695 0.383  9.643  0.000    2.944    4.445
#> 12   C2 ~1     -0.118 0.152 -0.775  0.438   -0.416    0.180
#> 14   C2 ~~  C2  4.399 0.464  9.474  0.000    3.489    5.309
#> 19   C3 ~1      0.107 0.148  0.726  0.468   -0.182    0.397
#> 21   C3 ~~  C3  4.079 0.431  9.470  0.000    3.234    4.923
#> 26   C4 ~1      0.075 0.154  0.486  0.627   -0.227    0.377
#> 28   C4 ~~  C4  4.189 0.466  8.989  0.000    3.275    5.102
#> 33   C5 ~1      0.049 0.158  0.311  0.756   -0.260    0.358
#> 35   C5 ~~  C5  4.339 0.480  9.048  0.000    3.399    5.279
#> 40   C6 ~1     -0.023 0.151 -0.153  0.879   -0.319    0.273
#> 42   C6 ~~  C6  3.962 0.431  9.203  0.000    3.118    4.806
#> 47   C7 ~1     -0.095 0.143 -0.669  0.504   -0.375    0.184
#> 49   C7 ~~  C7  3.457 0.381  9.078  0.000    2.711    4.203
#> 54   C8 ~1      0.107 0.140  0.764  0.445   -0.168    0.382
#> 56   C8 ~~  C8  3.373 0.369  9.152  0.000    2.651    4.095
#> 61   C9 ~1      0.031 0.152  0.206  0.836   -0.267    0.330
#> 63   C9 ~~  C9  4.201 0.448  9.375  0.000    3.323    5.079
#> 416  F1 =~  C2 -0.058 0.479 -0.122  0.903   -0.997    0.880
#> 417  F1 =~  C3  1.055 0.562  1.875  0.061   -0.048    2.157
#> 419  F2 =~  C5  1.116 0.694  1.607  0.108   -0.245    2.477
#> 420  F2 =~  C6  1.208 0.739  1.635  0.102   -0.240    2.656
#> 422  F3 =~  C8 -0.010 0.342 -0.030  0.976   -0.681    0.661
#> 423  F3 =~  C9  1.307 0.567  2.304  0.021    0.195    2.419
#> 424  F1 ~~  F2  0.264 0.152  1.741  0.082   -0.033    0.561
#> 425  F1 ~~  F3  0.422 0.213  1.988  0.047    0.006    0.839
#> 426  F2 ~~  F3  0.376 0.192  1.955  0.051   -0.001    0.752
```

To compare estimates of common parameters in TS and PIM, the user can
use the `compare_est` function available in `twostage`:

``` r
comp_table <- compare_est(fit_pim,fit_ts)

#rounding numeric columns to three decimal places
as.data.frame(lapply(comp_table, function(x) {
  if(is.numeric(x)) { round(x, 3) } else {x} }))
#>    lhs op rhs est.fit_pim se.fit_pim z.fit_pim pvalue.fit_pim est.fit_ts
#> 1   C1 ~~  C1       3.695      0.383     9.643          0.000      3.019
#> 2   C1 ~1           0.258      0.141     1.833          0.067      0.260
#> 3   C2 ~~  C2       4.399      0.464     9.474          0.000      3.681
#> 4   C2 ~1          -0.118      0.152    -0.775          0.438     -0.116
#> 5   C3 ~~  C3       4.079      0.431     9.470          0.000      2.787
#> 6   C3 ~1           0.107      0.148     0.726          0.468      0.113
#> 7   C4 ~~  C4       4.189      0.466     8.989          0.000      3.573
#> 8   C4 ~1           0.075      0.154     0.486          0.627      0.084
#> 9   C5 ~~  C5       4.339      0.480     9.048          0.000      3.458
#> 10  C5 ~1           0.049      0.158     0.311          0.756      0.057
#> 11  C6 ~~  C6       3.962      0.431     9.203          0.000      2.919
#> 12  C6 ~1          -0.023      0.151    -0.153          0.879     -0.029
#> 13  C7 ~~  C7       3.457      0.381     9.078          0.000      2.743
#> 14  C7 ~1          -0.095      0.143    -0.669          0.504     -0.114
#> 15  C8 ~~  C8       3.373      0.369     9.152          0.000      3.167
#> 16  C8 ~1           0.107      0.140     0.764          0.445      0.112
#> 17  C9 ~~  C9       4.201      0.448     9.375          0.000      3.137
#> 18  C9 ~1           0.031      0.152     0.206          0.836      0.032
#> 19  F1 ~~  F2       0.264      0.152     1.741          0.082      0.343
#> 20  F1 ~~  F3       0.422      0.213     1.988          0.047      0.332
#> 21  F1 =~  C2      -0.058      0.479    -0.122          0.903      1.022
#> 22  F1 =~  C3       1.055      0.562     1.875          0.061      1.389
#> 23  F2 ~~  F3       0.376      0.192     1.955          0.051      0.590
#> 24  F2 =~  C5       1.116      0.694     1.607          0.108      1.198
#> 25  F2 =~  C6       1.208      0.739     1.635          0.102      1.292
#> 26  F3 =~  C8      -0.010      0.342    -0.030          0.976      0.503
#> 27  F3 =~  C9       1.307      0.567     2.304          0.021      1.214
#>    se.fit_ts z.fit_ts pvalue.fit_ts
#> 1      0.417    7.242         0.000
#> 2      0.141    1.843         0.065
#> 3      0.495    7.435         0.000
#> 4      0.152   -0.761         0.447
#> 5      0.570    4.885         0.000
#> 6      0.148    0.761         0.446
#> 7      0.472    7.572         0.000
#> 8      0.155    0.541         0.589
#> 9      0.501    6.900         0.000
#> 10     0.158    0.363         0.717
#> 11     0.462    6.313         0.000
#> 12     0.152   -0.191         0.849
#> 13     0.413    6.639         0.000
#> 14     0.143   -0.797         0.426
#> 15     0.368    8.606         0.000
#> 16     0.140    0.798         0.425
#> 17     0.529    5.935         0.000
#> 18     0.154    0.208         0.835
#> 19     0.161    2.123         0.034
#> 20     0.166    1.998         0.046
#> 21     0.379    2.696         0.007
#> 22     0.508    2.736         0.006
#> 23     0.222    2.653         0.008
#> 24     0.420    2.849         0.004
#> 25     0.433    2.987         0.003
#> 26     0.241    2.082         0.037
#> 27     0.401    3.027         0.002
```

The only complicating element for TS and PIM methods is approximate fit
assessment; this aspect is under development, see Approximate_fit
vignette. When data are complete, both TS and PIM produce equivalent
output to the complete data run on the manually-formed composites, see
the Complete_data vignette.

<!-- Links to (../doc/Complete_data.html) do not work until the package website is on Github. Links to  (../vignettes/Complete_data.html) do not work because this folder does not contain hmtl files. Ignore for now, see advice below.
&#10;<!-- Note on GitHub: If the README is on GitHub and you want to link to the rendered vignette during development, you could manually provide a link to a rendered version stored externally (e.g., on GitHub Pages) until it is published on CRAN.-->
