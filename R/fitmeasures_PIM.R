## fit measures for PIM

# simplification of lav_fit_srmr_mplus, available in lav_fit_srmr.R
# modified to have dependence on two lavaan objects and the compmodel
# the harmelss dependence on G partially left, for (hopefully) a future generalization

#' Title
#'
#' @param lavobject  Estimated PIM (H0)
#' @param lavobject_sat Estimated saturated PIM (H1)
#' @param compmodel Composites Model
#'
#' @returns A list of SRMR values, without means and with means
#' @export
#'
#' @examples
#' # make a non-boring example, here H0 is saturated
#' library(lavaan)
#' misdata_1to7 <- misdata_mcar20[, 1:7] # first 7 vars, for illustration
#' # C1 is the sum of Y1, Y2, and Y3
#' # C2 is the sum of Y4, Y5, and Y6
#' # C3 is Y7
#' C <- matrix(0, nrow = 3, ncol = 7)
#' C[1, 1:3] <- 1
#' C[2, 4:6] <- 1
#' C[3, 7] <- 1
#' rownames(C) <- c("C1", "C2", "C3")
#' colnames(C) <- c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7")
#' compmodel <- "C1 ~ C2 + C3"
#' PIM_model <- PIM_syntax(compmodel = compmodel, C = C)
#' PIM_model_sat <- PIM_syntax_sat(compmodel = compmodel,C = C)
#' fit_pim <- lavaan(PIM_model, data = misdata_1to7)
#' fit_pim_sat <- lavaan(PIM_model_sat, data = misdata_1to7)
#' srmr <- srmr_mplus_pim(fit_pim, fit_pim_sat, compmodel)
srmr_mplus_pim <- function(lavobject, lavobject_sat, compmodel) {
  comp_names <- lavNames(compmodel) # variables to use in SRMR

  # ngroups
  G <- lavobject@Data@ngroups

  # container per group -- each a vector of length G initialized to 0
  srmr_mplus.group <- numeric(G)
  srmr_mplus_nomean.group <- numeric(G)

  for (g in 1:G) {
    # H1 (sat), EM estimates
    # S <- lavobject@SampleStats@missing.h1[[g]]$sigma # but I need names...
    # M <- lavobject@SampleStats@missing.h1[[g]]$mu
    # comp data
    # S <- lavobject@SampleStats@cov[[g]] #but I need names!
    # M <- lavobject@SampleStats@mean[[g]]

    # Is lavNames(lavobject) guaranteed to retrieve them in the right order?
    # Because I need to a subset of them, this is really important.
    # Relying on lavInspect for now (lost generalization to MG from here on)

    # saturated H1 model, based on lavobject_sat
    S <- lavInspect(lavobject_sat, "cov.all")[comp_names, comp_names, drop = FALSE]
    M_lv <- lavInspect(lavobject_sat, "mean.lv")
    M_ov <- lavInspect(lavobject_sat, "mean.ov")
    M_all <- c(M_lv, M_ov)
    M <- M_all[comp_names, drop = FALSE]

    nvar <- ncol(S)

    # H0, estimated model, based on lavobject
    # implied <- lavobject@implied    #but I need variable names!
    # lavmodel <- lavobject@Model
    # Sigma.hat <- implied$cov[[g]]
    # Mu.hat <- implied$mean[[g]]

    Sigma.hat <- lavInspect(lavobject, "cov.all")[comp_names, comp_names, drop = FALSE]
    Mu_lv <- lavInspect(lavobject, "mean.lv")
    Mu_ov <- lavInspect(lavobject, "mean.ov")
    Mu_all <- c(Mu_lv, Mu_ov)
    Mu.hat <- Mu_all[comp_names, drop = FALSE]

    # Bollen approach: simply using cov2cor ('correlation residuals')
    S.cor <- stats::cov2cor(S)
    Sigma.cor <- stats::cov2cor(Sigma.hat)
    R.cor <- (S.cor - Sigma.cor)

    # meanstructure
    if (lavobject@Model@meanstructure) {
      # standardized residual mean vector
      R.cor.mean <- M / sqrt(diag(S)) - Mu.hat / sqrt(diag(Sigma.hat))

      e <- nvar * (nvar + 1) / 2 + nvar
      srmr_mplus.group[g] <-
        sqrt((sum(R.cor[lower.tri(R.cor, diag = FALSE)]^2) +
          sum(R.cor.mean^2) +
          sum(((diag(S) - diag(Sigma.hat)) / diag(S))^2)) / e)

      e <- nvar * (nvar + 1) / 2
      srmr_mplus_nomean.group[g] <-
        sqrt((sum(R.cor[lower.tri(R.cor, diag = FALSE)]^2) +
          sum(((diag(S) - diag(Sigma.hat)) / diag(S))^2)) / e)
    } else {
      e <- nvar * (nvar + 1) / 2
      srmr_mplus_nomean.group[g] <- srmr_mplus.group[g] <-
        sqrt((sum(R.cor[lower.tri(R.cor, diag = FALSE)]^2) +
          sum(((diag(S) - diag(Sigma.hat)) / diag(S))^2)) / e)
    }
  } # G

  out <- list(srmr_mplus_nomean.group, srmr_mplus.group)
  # what happens if no mean structure? not our problem, but does the value stay at 0
  names(out) <- c("srmr_mplus_nomean", "srmr_mplus")
  return(out)
}


## Fit Measures function for PIM that automates the computation of CFI/TLI and SRMR (it fits the baseline and the saturated models for PIM to do so)
## This one needs a lot of error catching, should models fail to converge, etc
## If only fit_pim is fed in, we can extract compmodel... but we cannot extract C
## Maybe we can rewrite PIM_syntax_base and PIM_syntax_sat to only rely on fit_pim

#' PIM fit measures based on correct baseline and saturated models
#'
#' Returns PIM fit measures, where incremental fit indices have been adjusted to be based on a custom PIM baseline model, and where SRMR indices have been adjusted to be based on a custom H1 (saturated) model
#' Indices that have not been adjusted and should not be interpreted for PIM are omitted (for now)
#' RMSEA is already correct
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are
#'   components
#' @param compmodel The lavaan model for the composites
#' @param fit_pim A lavaan object for the fitted PIM model
#' @param data Data file for the components (items)
#' @param exog_cov Should exogenous variables in the baseline model for composites be correlated?
#'
#' @returns a fitMeasures vector of lavaan, truncated to include only indices that can be interpreted (are already correct or have been adjusted)
#' @export
#'
#' @examples
#' # An example using the first 18 variables in the simulated
#' # dataset misdata_mcar20 with 20% missing data on about half the variables
#' # C1 - C6 are parcels formed using three variables each (in order)
#'
#' library(lavaan)
#' misdata1 <- misdata_mcar20[, 1:18]
#'
#' # composite sub-model
#' mod1 <- "
#'  F1 =~ C1 + C2 + C3
#'  F2 =~ C4 + C5 + C6
#'  F2 ~ F1
#'  F2 ~~ F2
#'  F1 ~~ F1"

#' #manual computation for C to avoid user input
#' cnames<-lavNames(mod1) #this is a lavaan function
#' C <- matrix(0,nrow=length(cnames),ncol=length(colnames(misdata1)))
#' colnames(C)<-colnames(misdata1)
#' rownames(C)<-cnames
#' C[1,1:3]<-1
#' C[2,4:6]<-1
#' C[3,7:9]<-1
#' C[4,10:12]<-1
#' C[5,13:15]<-1
#' C[6,16:18]<-1
#'
#' pim_mod1 <- PIM_syntax(mod1,C)
#' fit_pim <- lavaan::lavaan(pim_mod1, data=misdata1)
#'
#' fitMeasures_pim(C = C, compmodel = mod1, fit_pim = fit_pim,
#' data = misdata1, exog_cov = TRUE)
#'
fitMeasures_pim <- function(C, compmodel, fit_pim, data = data, exog_cov = TRUE) {
  # step1: create baseline model syntax
  compmodel_base <- PIM_syntax_base(
    C = C, compmodel = compmodel,
    exog_cov = exog_cov
  )
  # step 2: fit the baseline model
  fit_pim_base <- lavaan(compmodel_base, data = data)
  # step 3: get custom fit measures from lavaan (could take awhile)
  fits_pim <- fitMeasures(fit_pim, baseline.model = fit_pim_base)
  # step 4: create H1 model syntax
  compmodel_sat <- suppressMessages(PIM_syntax_sat(C = C, compmodel = compmodel))
  # step 5: fit the saturated (H1) model:
  fit_pim_sat <- lavaan::lavaan(compmodel_sat, data = data)
  srmr <- srmr_mplus_pim(fit_pim, fit_pim_sat, compmodel)
  fits_pim["srmr_mplus"] <- srmr$srmr_mplus
  fits_pim["srmr"] <- srmr$srmr_mplus # assume a mean structure
  fits_pim["srmr_mplus_nomean"] <- srmr$srmr_mplus_nomean

  # names to keep

  fits_names_norobust <- c(
    "npar", "fmin", "chisq", "df", "pvalue",
    "baseline.chisq", "baseline.df", "baseline.pvalue",
    "cfi", "tli", "nnfi", "rfi", "nfi", "pnfi", "ifi", "rni", "logl", "unrestricted.logl", "aic", "bic", "ntotal", "bic2", "rmsea", "rmsea.ci.lower",
    "rmsea.ci.upper", "rmsea.ci.level", "rmsea.pvalue", "rmsea.close.h0", "rmsea.notclose.pvalue", "rmsea.notclose.h0", "srmr", "srmr_mplus", "srmr_mplus_nomean"
  )

  fits_names_robust <- c(
    "cfi.robust", "tli.robust", "nnfi.robust", "rni.robust",
    "rmsea.robust",
    "rmsea.ci.lower.robust", "rmsea.ci.upper.robust",
    "rmsea.pvalue.robust", "rmsea.notclose.pvalue.robust"
  )

  all_exist <- all(fits_names_robust %in% names(fits_pim))
  if (all_exist) {
    fits_names <- c(fits_names_norobust, fits_names_robust)
  } else {
    fits_names <- fits_names_norobust
  }

  return(round(fits_pim[fits_names], 3))
}
