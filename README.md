
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
sums or averages of the items; the weights are all equal (for now) and
are fixed rather than estimated.

These methods can be contrasted with confirmatory composite analysis
(CCA) of [Henseler
(2021)](https://www.guilford.com/books/Composite-Based-Structural-Equation-Modeling/J%C3%B6rg-Henseler/9781462545605),
which 1) finds optimally weighted composites (for prediction), and 2)
does not allow indicators to influence other variables directly (but
only through the composite). This approach will *not* return identical
results to composite models with complete data. CCA is currently being
implemented in `lavaan` (but with some limitations for missing data
treatment).

## Installation

To install twostage from [GitHub](https://github.com/):

``` r
install.packages("pak")
pak::pak("vsavalei/twostage")
```

Load it in the usual way:

``` r
library(twostage) 
#> This package is written by a newbie. You've been warned.
```

## Example

This example uses a simulated dataset `misdata_mcar20`, which contains
27 items, $`Y_1`$ to $`Y_{27}`$, where about half have 20% missing data.
The model is for composites $`C_1`$ to $`C_9`$, which are parcels of
three items each, in order; for example, $`C_1 = Y_1 + Y_2 + Y_3`$, and
so on. These composites are never explicitly computed.

The composite model is a 3-factor model, with three indicators each,
defined via `lavaan` syntax as follows:

``` r
#composite model
mod <- '
F1 =~ 1*C1 + C2 + C3
F2 =~ 1*C4 + C5 + C6
F3 =~ 1*C7 + C8 + C9
F1 ~~ F1
F2 ~~ F2
F3 ~~ F3
'
```

For now, for true latent variables (*not* composites), the user should
set their metric and variances explicitly in the composite model syntax.
Above, the first loading of each true latent variable (F1, F2, F3) is
set to 1, and their variances are free. This is because the PIM model
will be fit via the `lavaan` function, which assumes nothing. The
`PIM_syntax` automation function *will* correlate all exogenous
variables in the model by default, so it is not necessary to specify,
e.g., `F1 ~~ F2` in the composite model syntax (though it doesn’t hurt).
In general, the user should aim to specify the composite model as
explicitly and thoroughly as possible.

To fit this composite model using the item-level methods in this
package, the assignment of components to composites needs to be
specified. To do this via an interactive interface, use:

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

The created matrix $`C`$ has columns labeled with component names:
$`Y_1`$ to $`Y_{27}`$, and rows labeled with composite names: $`C_1`$ to
$`C_9`$. The ijth element of $`C`$ is nonzero (for sums, it is 1) if
component $`j`$ belongs to composite $`i`$, and zero otherwise.

In this file, we create this matrix manually as follows:

Once the $`C`$ matrix is created with the help of the `stage0` function
or manually, the composite model can be fit using the methods included
in the package.

To fit the composite model using TSML:

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
#>   F1 ~~  F1  0.676    0.327   2.066        0.039 0.344  1.964  0.050
#>   F2 ~~  F2  0.649    0.308   2.107        0.035 0.346  1.877  0.060
#>   F3 ~~  F3  0.760    0.322   2.358        0.018 0.367  2.070  0.038
#>   C1 ~~  C1  3.019    0.391   7.724        0.000 0.417  7.242  0.000
#>   C2 ~~  C2  3.681    0.456   8.077        0.000 0.495  7.435  0.000
#>   C3 ~~  C3  2.787    0.520   5.354        0.000 0.570  4.885  0.000
#>   C4 ~~  C4  3.573    0.415   8.619        0.000 0.472  7.572  0.000
#>   C5 ~~  C5  3.458    0.437   7.907        0.000 0.501  6.900  0.000
#>   C6 ~~  C6  2.919    0.415   7.041        0.000 0.462  6.313  0.000
#>   C7 ~~  C7  2.743    0.363   7.559        0.000 0.413  6.639  0.000
#>   C8 ~~  C8  3.167    0.333   9.504        0.000 0.368  8.606  0.000
#>   C9 ~~  C9  3.137    0.465   6.746        0.000 0.529  5.935  0.000
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
greater uncertainty due to missing data in Stage 1. The (normal theory)
residual-based test statistic is also printed. For technical details on
the standard errors and the residual-based test statistic computation,
see [Savalei and Bentler (
2009)](https://www.tandfonline.com/doi/full/10.1080/10705510903008238)
and [Savalei and Rhemtulla
(2017a)](https://journals.sagepub.com/doi/full/10.3102/1076998617694880).

To fit the composite model using PIM, first create the `lavaan` PIM
syntax:

``` r
modpim <- PIM_syntax(compmodel = mod, C = C)
#> The following exogeneous variables in the composites model will be  correlated by default:F1, F2, F3. 
#> If you do not want this, modify the composite model syntax manually or set exog_cov=FALSE.
```

The resulting syntax is long and can be viewed via `cat(modpim)`. It
contains the definition of each composite $`C_i`$, $`i=1,\ldots,9`$, as
a single-indicator latent variable, and a special structure on the
components. For details, see [Rose, Wagner, Mayer, and Nagengast
(2019).](https://online.ucpress.edu/collabra/article/5/1/9/112958/Model-Based-Manifest-and-Latent-Composite-Scores)
It is recommend to always check the part of the generated syntax
pertaining to the composites (clearly marked at the bottom). The default
in `PIM_syntax` is to covary exogenous variables (whether observed
variables, latent variables, or composites); to undo this default, use
`exog_cov=FALSE`.

The PIM model can be fit directly in `lavaan`. To treat missing data on
the items, use FIML:

``` r
fit_pim <- lavaan::lavaan(modpim, data=misdata_mcar20,missing="FIML")
fit_pim
#> lavaan 0.6-20.2318 ended normally after 260 iterations
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

While the `sem` function would work in this case, it is best to use the
`lavaan()` function to fit PIMs to avoid any unintended defaults, given
the nonstandard setup of this model. (And until we learn more about it).

Because the PIM setup adds items to the composites model in a saturated
way, always confirm that the degrees of freedom are what you would
expect for your composite model (i.e., if data had been complete and the
composite model had been fit directly). In this example, the composite
model is a 3-factor CFA with nine indicators, so it should have
$`9(10)/2 - (9+9+3)=24`$ degrees of freedom, where the parameters are
free factor loadings, residual variances, and factor variances and
covariances. When fit correctly,`fit_pm` should also return the same df
as `fit_ts`.

To inspect parameter estimates, use `summary(fit_pim)` or the
`parameterEstimates` function of `lavaan`. As the interest is in the
composite model, the user has to ignore a lot of extraneous output
pertaining to the item parameters (here, all rows referring to “Y”
variables).

To compare estimates of common parameters in TS and PIM, which are those
in the composite model only, we can use the `compare_est` function:

``` r
comp_table <- compare_est(fit_pim,fit_ts)
comp_table
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
#> 21  F1 ~~  F3       0.312      0.171     1.823          0.068      0.332
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

The TS and PIM estimates and standard errors are quite similar.
Technically, the TS estimates are less efficient than FIML (PIM)
estimates. When data are complete, both TS and PIM produce equivalent
output to the complete data run on the manually-formed composites (see
the Complete_data vignette).

Approximate fit assessment of PIM models requires special setup, as
discussed in [Rose, Wagner, Mayer, and Nagengast
(2019).](https://online.ucpress.edu/collabra/article/5/1/9/112958/Model-Based-Manifest-and-Latent-Composite-Scores).
The gist of the issue is that we want to assess the fit of the composite
model, not the fit of the entire model with items added in a saturated
way; this requires recomputing CFI and SRMR (but RMSEA is fine). To get
corrected fit measures, use:

``` r
fitm_pim <- fitMeasures_pim(C, compmodel=mod, fit_pim=fit_pim, 
                data = misdata_mcar20)
```

We can compare the PIM and TS fit indices:

``` r
indices<-c("rmsea","rmsea.robust","cfi","cfi.robust","tli","tli.robust","srmr")
fitm_pim[indices]
#>        rmsea rmsea.robust          cfi   cfi.robust          tli   tli.robust 
#>        0.008        0.024        0.978        0.996        0.967        0.994 
#>         srmr 
#>        0.131

lavaan::fitMeasures(fit_ts,indices)
#> rmsea   cfi   tli  srmr 
#> 0.044 0.913 0.869 0.047
```

When `estimator ="FIML"`, as in PIM, corrections are necessary to fit
indices in order for them to mimic their would-be complete data values
[Zhang and Savalei
(2023)](https://psycnet.apa.org/doiLanding?doi=10.1037%2Fmet0000445).
Thus, we compare the RMSEA from `fit_ts` to `rmsea.robust` from
`fit_pim`. Similarly, we should compare the CFI from `fit_ts` to the
`cfi_robust` in `fit_pim`, but this CFI is custom-computed using a
different baseline model, so it is not clear if these corrections are
right. No corrections are necessary to SRMR, so these can be compared
directly. See the [Approximate_fit
vignette](../doc/Approximate_fit.html) for more detail. Lastly, TSML
indices require small-sample corrections (these are less consequential
than PIM corrections, as they go away asymptotically), but these have
not yet been implemented.

<!-- Links to (../doc/Complete_data.html) do not work until the package website is on Github. Links to  (../vignettes/Complete_data.html) do not work because this folder does not contain hmtl files. Ignore for now, see advice below.
&#10;<!-- Note on GitHub: If the README is on GitHub and you want to link to the rendered vignette during development, you could manually provide a link to a rendered version stored externally (e.g., on GitHub Pages) until it is published on CRAN.-->
