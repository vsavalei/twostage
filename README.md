
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
```

\[Describe output once `summary(out_ts)` is available\]

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
#> lavaan 0.6-20.2276 ended normally after 261 iterations
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

\[Describe output\]
