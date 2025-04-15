# Comparison of output from different methods for fitting the same model
# Only common parameters are extracted and compared
# All input should be either lavaan or twostage objects
# Q: Remove nonconverged? (semTools does this)
# Eventually add a check that the same comp model was fit to the same data

#' Compare Estimates of Common Parameters
#'
#'
#' @param ... objects (lavaan or twostage)

#'
#' @returns A data frame with estimates and SEs from both models for common model parameters
#' in the style of `parameterestimates()` of `lavaan`
#' @export
#'

# much taken from net in semTools
compare_est<- function (...) {

  fitList <- list(...)

  ## check that they are all lavaan objects
  notLavaan <- !sapply(fitList, inherits, what = "lavaan")
  if (any(notLavaan)) {
    fitNames <- sapply(as.list(substitute(list(...)))[-1], deparse)
    stop(paste("The following arguments are not fitted lavaan objects:\n",
               paste(fitNames[notLavaan], collapse = "\t")))
  }

  # Create a sublist with only lavaan objects
  lavList <- Filter(function(x) class(x)[1] == "lavaan", fitList)

  # Create a sublist with only twostage objects
  tsList <- Filter(function(x) "twostage" %in% class(x), fitList)

  #compute the parameter estimates table for each lavaan object
  lavResults <- lapply(lavList, parameterEstimates,
                       ci=FALSE,remove.nonfree=TRUE)

  #compute the parameter estimates table for each twostage object
  tsResults <- lapply(tsList, parameterEstimates_ts,naive.se=FALSE)

  Results <- c(lavResults, tsResults)

  # create a key column to identify common parameters
  Results <- lapply(Results, function(df) {
    df$key <- paste(df$lhs, df$op, df$rhs, sep = "") # create key
    # Optional: remove the original 'lhs', 'op', and 'rhs' columns
    df <- df[, !(names(df) %in% c("lhs", "op", "rhs"))]
    return(df)
  })

  #creating a single data frame (deal with names later)
  Results_merged <- Reduce(function(x, y) merge(x, y, by = "key"), Results)

 return(Results_merged) #just need names
}
