# Comparison of output from different methods for fitting the same model
# Only common parameters are extracted and compared
# All input should be either lavaan or twostage objects
# TODO: If twostage class is not used, simplify
# Q: Remove nonconverged? (semTools does this)
# Eventually add a check that the same comp model was fit to the same data
# parts have been taken from net in semTools
# TODO: Add an example

#' Compare Estimates of Common Parameters from Multiple Runs
#'
#'
#' @param ... Any number of objects of class lavaan or twostage

#'
#' @returns A data frame with estimates, standard errors, z-statistics, and p-values
#' from all runs for the common model parameters, in the style of `parameterestimates()` of `lavaan`
#' @export
#'


compare_est <- function(...) {
  fitList <- list(...)
  fit_names <- sapply(substitute(list(...))[-1], deparse)
  names(fitList) <- fit_names

  ## check that they are all lavaan objects
  notLavaan <- !sapply(fitList, inherits, what = "lavaan")
  if (any(notLavaan)) {
    fitNames <- sapply(as.list(substitute(list(...)))[-1], deparse)
    stop(paste(
      "The following arguments are not fitted lavaan objects:\n",
      paste(fitNames[notLavaan], collapse = "\t")
    ))
  }

  # Create a sublist with only lavaan objects
  lavList <- Filter(function(x) class(x)[1] == "lavaan", fitList)

  # Create a sublist with only twostage objects
  tsList <- Filter(function(x) "twostage" %in% class(x), fitList)

  # compute the parameter estimates table for each lavaan object
  lavResults <- lapply(lavList, parameterEstimates,
    ci = FALSE, remove.nonfree = TRUE
  )

  # compute the parameter estimates table for each twostage object
  # naive.se must be false or else additional columns will break the naming
  tsResults <- lapply(tsList, parameterEstimates_ts, naive.se = FALSE)

  Results <- c(lavResults, tsResults)

  # Iterate over the list and rename columns by appending run names.e.g, ".fit1"
  Results <- lapply(names(Results), function(name) {
    df <- Results[[name]]

    names(df) <- ifelse(names(df) %in% c("rhs", "lhs", "op"),
      names(df),
      paste0(names(df), ".", name)
    )
    return(df)
  })


  # creating a single data frame
  Results_merged <- Reduce(function(x, y) merge(x, y, by = c("lhs", "op", "rhs")), Results)

  # rounding to three decimal places, make a print method eventually
  Results_merged_rounded <- as.data.frame(lapply(Results_merged, function(x) {
    if (is.numeric(x)) {
      round(x, 3)
    } else {
      x
    }
  }))
  return(Results_merged_rounded)
}
