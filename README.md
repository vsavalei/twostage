
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- Render `README.Rmd` regularly to keep `README.md` up-to-date, via `devtools::build_readme()` -->

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
F1 =~ C1 + C2 + C3
F2 =~ C4 + C5 + C6
F3 =~ C7 + C8 + C9
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
#> Parameter estimates from Stage 2, naive standard errors from lavaan (with z-test and pvalue),
#>       and the corrected TSML standard errors (with z-test and p-value): 
#>  lhs op rhs         est  se_naive    z_naive pvalue_naive        se          z
#>   F1 =~  C2  1.02193658 0.3579328  2.8551076 4.302225e-03 0.3790369  2.6961397
#>   F1 =~  C3  1.38926575 0.4766224  2.9148140 3.559006e-03 0.5078380  2.7356473
#>   F2 =~  C5  1.19819027 0.3712583  3.2273766 1.249309e-03 0.4204921  2.8494955
#>   F2 =~  C6  1.29248625 0.3870511  3.3393170 8.398466e-04 0.4327614  2.9866025
#>   F3 =~  C8  0.50252629 0.2247838  2.2355987 2.537807e-02 0.2413742  2.0819390
#>   F3 =~  C9  1.21419672 0.3532458  3.4372577 5.876363e-04 0.4010741  3.0273622
#>   C1 ~~  C1  3.01850878 0.3907862  7.7241950 1.132427e-14 0.4168094  7.2419409
#>   C2 ~~  C2  3.68118084 0.4557431  8.0773149 6.661338e-16 0.4951376  7.4346624
#>   C3 ~~  C3  2.78671087 0.5204495  5.3544311 8.582601e-08 0.5704592  4.8850308
#>   C4 ~~  C4  3.57287005 0.4145500  8.6186714 0.000000e+00 0.4718739  7.5716626
#>   C5 ~~  C5  3.45781977 0.4373025  7.9071570 2.664535e-15 0.5011573  6.8996700
#>   C6 ~~  C6  2.91885671 0.4145512  7.0410035 1.908695e-12 0.4623456  6.3131496
#>   C7 ~~  C7  2.74306637 0.3628939  7.5588651 4.063416e-14 0.4131604  6.6392281
#>   C8 ~~  C8  3.16669743 0.3332076  9.5036785 0.000000e+00 0.3679581  8.6061365
#>   C9 ~~  C9  3.13697082 0.4649928  6.7462783 1.516853e-11 0.5285742  5.9347781
#>   F1 ~~  F1  0.67578595 0.3270536  2.0662848 3.880160e-02 0.3441623  1.9635678
#>   F2 ~~  F2  0.64868483 0.3078294  2.1072869 3.509272e-02 0.3455100  1.8774702
#>   F3 ~~  F3  0.76015959 0.3223813  2.3579522 1.837606e-02 0.3671408  2.0704852
#>   F1 ~~  F2  0.34270935 0.1495455  2.2916724 2.192455e-02 0.1614227  2.1230557
#>   F1 ~~  F3  0.33202639 0.1508528  2.2009957 2.773633e-02 0.1661643  1.9981811
#>   F2 ~~  F3  0.58960029 0.2008429  2.9356295 3.328717e-03 0.2222176  2.6532571
#>   C1 ~1      0.25996869 0.1359098  1.9128031 5.577326e-02 0.1410413  1.8432092
#>   C2 ~1     -0.11576178 0.1481037 -0.7816265 4.344341e-01 0.1520786 -0.7611973
#>   C3 ~1      0.11284636 0.1430213  0.7890179 4.301015e-01 0.1482134  0.7613774
#>   C4 ~1      0.08381759 0.1452851  0.5769178 5.639950e-01 0.1549407  0.5409657
#>   C5 ~1      0.05747750 0.1481403  0.3879936 6.980207e-01 0.1584999  0.3626344
#>   C6 ~1     -0.02888770 0.1414655 -0.2042031 8.381948e-01 0.1516308 -0.1905133
#>   C7 ~1     -0.11409123 0.1323485 -0.8620514 3.886592e-01 0.1431886 -0.7967898
#>   C8 ~1      0.11175214 0.1295890  0.8623581 3.884905e-01 0.1400749  0.7978030
#>   C9 ~1      0.03200253 0.1459050  0.2193381 8.263867e-01 0.1537131  0.2081965
#>        pvalue
#>  7.014823e-03
#>  6.225771e-03
#>  4.378863e-03
#>  2.820962e-03
#>  3.734804e-02
#>  2.466982e-03
#>  4.423129e-13
#>  1.048051e-13
#>  1.034127e-06
#>  3.685940e-14
#>  5.212275e-12
#>  2.734128e-10
#>  3.153300e-11
#>  0.000000e+00
#>  2.942432e-09
#>  4.958023e-02
#>  6.045367e-02
#>  3.840693e-02
#>  3.374918e-02
#>  4.569703e-02
#>  7.971913e-03
#>  6.529847e-02
#>  4.465393e-01
#>  4.464317e-01
#>  5.885312e-01
#>  7.168780e-01
#>  8.489069e-01
#>  4.255731e-01
#>  4.249848e-01
#>  8.350755e-01
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
```

The resulting syntax is long and can be viewed via `cat(modpim)`. It
contains the definition of each composite $C_i$, $i=1,\ldots,9$, as a
single-indicator latent variable, and a special structure on the items.
For details, see [Rose, Wagner, Mayer, and Nagengast
(2019).](https://online.ucpress.edu/collabra/article/5/1/9/112958/Model-Based-Manifest-and-Latent-Composite-Scores)

The PIM model can be fit directly in `lavaan`, using FIML to treat
missing data on the items:

``` r
fit_pim <- lavaan::sem(modpim, data=misdata_mcar20,missing="FIML")
fit_pim
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

ests <- lavaan::parameterestimates(fit_pim)
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

As the interest is in the composite model, the user just has to ignore a
lot of extraneous output pertaining to the item parameters. In the
output shown above, the output of the `parameterestimates` function is
modified by removing all rows referring to a “Y” variable (i.e., an
item).

To compare estimates of common parameters in TS and PIM, the user can
use built-in compare functions:

``` r
comp_table <- compare_est(fit_pim,fit_ts)

#rounding numeric columns to three decimal places
as.data.frame(lapply(comp_table, function(x) {
  if(is.numeric(x)) { round(x, 3) } else {x} }))
#>    lhs op rhs est.fit_pim se.fit_pim z.fit_pim pvalue.fit_pim est.fit_ts
#> 1   C1 ~~  C1       3.028      0.428     7.069          0.000      3.019
#> 2   C1 ~1           0.263      0.141     1.866          0.062      0.260
#> 3   C2 ~~  C2       3.590      0.505     7.106          0.000      3.681
#> 4   C2 ~1          -0.120      0.152    -0.789          0.430     -0.116
#> 5   C3 ~~  C3       2.833      0.574     4.934          0.000      2.787
#> 6   C3 ~1           0.115      0.148     0.774          0.439      0.113
#> 7   C4 ~~  C4       3.620      0.473     7.658          0.000      3.573
#> 8   C4 ~1           0.079      0.155     0.507          0.612      0.084
#> 9   C5 ~~  C5       3.393      0.497     6.828          0.000      3.458
#> 10  C5 ~1           0.049      0.158     0.307          0.759      0.057
#> 11  C6 ~~  C6       2.997      0.471     6.366          0.000      2.919
#> 12  C6 ~1          -0.031      0.152    -0.203          0.839     -0.029
#> 13  C7 ~~  C7       2.791      0.438     6.375          0.000      2.743
#> 14  C7 ~1          -0.119      0.144    -0.823          0.410     -0.114
#> 15  C8 ~~  C8       3.171      0.375     8.457          0.000      3.167
#> 16  C8 ~1           0.110      0.140     0.784          0.433      0.112
#> 17  C9 ~~  C9       3.189      0.512     6.227          0.000      3.137
#> 18  C9 ~1           0.038      0.153     0.248          0.804      0.032
#> 19  F1 ~~  F1       0.665      0.355     1.876          0.061      0.676
#> 20  F1 ~~  F2       0.339      0.163     2.074          0.038      0.343
#> 21  F1 ~~  F3       0.312      0.171     1.824          0.068      0.332
#> 22  F1 =~  C2       1.089      0.415     2.626          0.009      1.022
#> 23  F1 =~  C3       1.386      0.543     2.554          0.011      1.389
#> 24  F2 ~~  F2       0.642      0.342     1.881          0.060      0.649
#> 25  F2 ~~  F3       0.606      0.231     2.621          0.009      0.590
#> 26  F2 =~  C5       1.248      0.440     2.838          0.005      1.198
#> 27  F2 =~  C6       1.269      0.422     3.008          0.003      1.292
#> 28  F3 ~~  F3       0.779      0.393     1.984          0.047      0.760
#> 29  F3 =~  C8       0.511      0.242     2.113          0.035      0.503
#> 30  F3 =~  C9       1.173      0.414     2.831          0.005      1.214
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
#> 19     0.344    1.964         0.050
#> 20     0.161    2.123         0.034
#> 21     0.166    1.998         0.046
#> 22     0.379    2.696         0.007
#> 23     0.508    2.736         0.006
#> 24     0.346    1.877         0.060
#> 25     0.222    2.653         0.008
#> 26     0.420    2.849         0.004
#> 27     0.433    2.987         0.003
#> 28     0.367    2.070         0.038
#> 29     0.241    2.082         0.037
#> 30     0.401    3.027         0.002
```

The only complicating element for TS and PIM methods is approximate fit
assessment; this aspect is under development, see Approximate_fit
vignette. When data are complete, both TS and PIM produce equivalent
output to the complete data run on the manually-formed composites, see
the Complete_data vignette.

<!-- Links to (../doc/Complete_data.html) do not work until the package website is on Github. Links to  (../vignettes/Complete_data.html) do not work because this folder does not contain hmtl files. Ignore for now, see advice below.
&#10;<!-- Note on GitHub: If the README is on GitHub and you want to link to the rendered vignette during development, you could manually provide a link to a rendered version stored externally (e.g., on GitHub Pages) until it is published on CRAN.-->
