# Comparison of output from different methods
# Eventually input should be ..., but for now just two
# Eventually remove nonconverged (?)
# Eventually add a check that the same comp model was fit to the same data
# For TS, maybe everything should be grabbed from summary, duplicating work, here
# Generalize to include a lavaan run on composites computed by the user

#' Compare TSML and PIM Estimates and Fit
#'
#'
#' @param fitted_ts A twostage object
#' @param fitted_pim A lavaan object for a PIM model
#'
#' @returns A data frame with estimates and SEs from both models for the
#' composite-level model parameters (i.e., excluding item-level parameters of PIM),
#' in the style of `parameterestimates()` of `lavaan`, merged into one, with key.
#' @export
#'
#'
compare_est<- function (fitted_ts, fitted_pim) {

  ## check that fitted_pim is a lavaan object
   if (!inherits(fitted_pim, what = "lavaan")) {
        stop(paste("The fitted_pim argument is not a fitted lavaan object"))
  }

  if (!inherits(fitted_comp, what = "lavaan")) {
    stop(paste("The fitted_comp argument is not a fitted lavaan object"))
  }

  ## check that fitted_ts is a twostage object
  if (!inherits(fitted_ts, what = "twostage")) {
        stop(paste("The fitted_ts argument is not a fitted twostage object"))
  }

  ests_ts <- lavaan::parameterestimates(fitted_ts$TS_Run_naive) #TS estimates
  names(ests_ts)[names(ests_ts) == "se"] <- "se_naive.TS" #identifies naive SEs
  #TS parameter names key column added
  ests_ts$key <- paste(ests_ts$lhs, ests_ts$op, ests_ts$rhs, sep = "")
  ests_ts$se <- fitted_ts$TS_SEs[ests_ts$key] #adds TS SEs, checking for label


  ests_pim <- lavaan::parameterestimates(fitted_pim) #PIM estimates
  #TS parameter names key column added
  ests_ts$key <- paste(ests_ts$lhs, ests_ts$op, ests_ts$rhs, sep = " ")
  #PIM parameter names key column added
  ests_pim$key <- paste(ests_pim$lhs, ests_pim$op, ests_pim$rhs, sep = " ")

  ests_joint <- merge(ests_ts, ests_pim, by = "key",suffixes = c(".TS",".PIM"))
  return(ests_joint)
}
