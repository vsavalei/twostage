## JULY 2025: This class is no longer used (for now)
## File could be deleted (eventually)

## check via: ?`twostage-class`
#' Two-stage maximum likelihood estimation
#'
#' @description Objects returned by the twostage function, inheriting from lavaan
#' @slot twostage List containing TSML-specific information
#'
#' @section Methods:
#' \describe{
#'   \item{show}{Modifies lavaan's default display with Stage 1 and Stage 2 convergence info and TS test statistic information}
#'   \item{summary}{Summary of parameter estimates with naive and TSML standard errors, and test statistic summary output}
#'   \item{fitMeasures}{Fit measures output of lavaan is extended by adding two new elements (for now): residual-based TSML chi-square and its p-value}
#' }
#'
#' @name twostage-class
#' @exportClass twostage
NULL

setClass("twostage",
  contains = "lavaan",
  slots = list(twostage = "list")
)


# no need, make S3?
setClass("SummaryTwostage",
  slots = list(
    TS_table = "data.frame",
    Tres = "numeric",
    df = "numeric",
    pval = "numeric"
  )
)
