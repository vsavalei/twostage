# TSML functions #################

#' @import lavaan
#' @importFrom stats pnorm cov2cor
#' @importFrom methods show
#' @importFrom methods new
#' @importFrom lavaan fitMeasures
#' @importFrom methods callNextMethod
#' @importFrom utils capture.output

# namesohd (helper function): names rows/columns of the asy cov matrix from stage1
# update: changed order to match means, covs for NACOV argument
namesohd <- function(cnames) {
  name_grid_cov <- t(outer(cnames, cnames, function(x, y) paste0(x, "~~", y)))
  names_cov <- name_grid_cov[lower.tri(name_grid_cov, diag = TRUE)]
  names_mean <- paste0(cnames, "~", 1)
  namesohd <- c(names_mean, names_cov)
  return(namesohd)
}

# move to utils
# write_sat (helper function for both PIM and TS):
# creates the saturated model syntax in lavaan
# update: have means appear first, to create asy cov
# in the right order for NACOV argument in updated stage2
write_sat <- function(model = NULL, varnames = NULL) {
  # Get variable names from either source
  if (!is.null(model)) {
    varnames <- lavNames(model)
  } else if (is.null(varnames)) {
    stop("Must provide either 'model' or 'varnames'")
  }

  p <- length(varnames)
  sat.mod <- ""

  # Add means for all variables
  for (i in 1:p) {
    sat.mod <- paste(sat.mod, varnames[i], " ~ 1 \n ", sep = "")
  }

  # Variance and covariance terms
  for (i in 1:p) {
    linestart <- paste(varnames[i], " ~~ ", varnames[i], sep = "")
    if (p - i > 0) {
      linemid <- ""
      for (j in (i + 1):p) {
        linemid <- paste(linemid, " + ", varnames[j], sep = "")
      }
    } else {
      linemid <- ""
    }
    sat.mod <- paste(sat.mod, linestart, linemid, " \n ", sep = "")
  }


  return(sat.mod)
}

#' stage0
#'
#' Prep stage for composite-based methods (TS and PIM). This function uses
#' p column names of data and k composite variable names from the lavaan
#' model to create a k x p matrix C, with nonzero elements representing the assignment
#' of components to composites. Which elements are nonzero is determined by presenting
#' a menu of composite names to the user for each component. What the weights are is
#' determined by presenting the user with the menu to create composites as sum scores
#' or average scores.
#'
#' @param data A data file with components (items) to be assigned to composites.
#' It should not contain other variables (or else feed in only selected columns as data).
#' @param compmodel A lavaan model for the composites, used to extract composite names
#' @param type  Types of composites: 1 for sums, 2 for averages (or leave blank to input interactively)
#' @param which_col (optional) A vector of length of names(data), with entries identifying the number of the composite
#' (in the order they appear in the composites model) to which a component should be signed
#' @return A matrix C with rownames set to composite names, and columnnames set to
#' component (item) names. A non-zero value in each row corresponds to the assignment of that
#' component to that composite, with the non-zero value as the weight. Current options for weights
#' are 1 for unit-weighted and 1/pj for average-weighted, where pj is the number of components in that
#' composite.
#'
#' @export
#'
#' @examples
#'
#' # TPB Model for Composites (Full mediation)
#' tpbmod <- "
#' INTALL ~ ATTALL + PBCALL + NORMALL
#' BEH ~ INTALL"
#' \dontrun{
#' # The menu cannot be used non-interactively
#' C <- stage0(tpbdata, tpbmod)
#' }
#' # Or, provide assignment non-interactively
#' # The composites are in the order listed in lavNames(tpbmod)
#' # The components are in the order of names(tpbdata)
#  # Therefore, the correct assignment vector is:
#' which_col <- c(rep(4, 3), rep(5, 3), rep(2, 1), rep(1, 3), rep(3, 11))
#' C <- stage0(tpbdata, tpbmod, which_col = which_col, type = 1)
#'
#' # Your composites are made up of the following components:
#' # INTALL :  INT1 INT2 INT3
#' # BEH :  BEH
#' # ATTALL :  AT1CPU AT2CPU AT3CPU AT4CPU AT5CPU AT6CPU AT7CPP AT8CPP AT9CPP AT10CPP AT11CPP
#' # PBCALL :  PBC1 PBC2 PBC3
#' # NORMALL :  NORS1 NORS2 NORS3
#' # If this is not correct, start over!
#'
stage0 <- function(data, compmodel, which_col = NA, type = NA) {
  cnames <- lavNames(compmodel)
  C <- matrix(0, nrow = length(cnames), ncol = length(colnames(data)))
  colnames(C) <- colnames(data) # component names
  rownames(C) <- cnames # composite names

  if (!(type %in% c(1, 2))) {
    prompt_type <- paste("Are your composites sums or averages of components?")
    type <- utils::menu(c("Sums", "Averages"), title = prompt_type)
  }

  if (!missing(which_col)) { # use assignment vector if provided
    # if which_col is not a vector of length of colnames(data), throw error
    if (length(which_col) != length(colnames(data))) {
      stop(
        "'which_col' must have length ", length(colnames(data)),
        " (number of columns in data), but has length ", length(which_col)
      )
    }

    # if which_col has any values other than 1 to length(cnames), throw error
    if (any(!which_col %in% seq_len(length(cnames)))) {
      stop("Error: Values in 'which_col' must be integers between 1
           and the number of variables in compmodel.")
    }

    for (i in seq_along(colnames(data))) {
      ind_i <- which_col[i]
      C[ind_i, i] <- 1
    }
  } else { # menu prompt for each component
    for (i in seq_along(colnames(data))) {
      prompt_message <- paste("Please select the composite for variable", colnames(data)[i], ":")
      ind_i <- utils::menu(cnames, title = prompt_message)
      C[ind_i, i] <- 1
    }
  }

  cat("Your composites are made up of the following components: \n")
  for (j in seq_along(cnames)) {
    cnamesj <- colnames(C)[C[j, ] == 1]
    cat(cnames[j], ": ", cnamesj, "\n")
  }
  cat("If this is not correct, start over! \n")

  if (type == 2) { # rescale unit weights (changed from "2" to 2)
    C <- C / rowSums(C)
  }

  return(C)
}



#' stage1
#'
#' Stage1 of TSML: fits a saturated model to the items with missing data
#'
#' @param data A datafile with components (items).
#' It should not contain any other variables.
#'
#' @param runcommand Additional arguments to pass to lavaan
#'
#' @return  A list with 4 objects: N (sample size), mhb (estimated means),
#' shb (estimated covariance matrix), ohb (estimated asymptotic covariance matrix
#' of mhb and shb elements)
#'
#' @export
#'
#' @examples
#'
#' # default estimator is FIML:
#' out_s1 <- stage1(misdata_mcar20)
#'
#' # requesting robust (to nonnormality) estimator:
#' out_s1 <- stage1(misdata_mcar20, runcommand = 'estimator="MLR"')
#'
#' # complete data example:
#' # running stage1 with expected information
#' # will result in TSML matching regular ML run on composites
#' out_s1 <- stage1(tpbdata, runcommand = "information='expected'")
#'
stage1 <- function(data, runcommand = NULL) {
  # Validate runcommand argument
  if (!is.null(runcommand)) {
    if (!is.character(runcommand) || length(runcommand) != 1) {
      stop("'runcommand' must be a single character string or NULL", call. = FALSE)
    }

    # Check for potentially problematic specifications
    invalid_patterns <- list(
      data = "data\\s*=",
      model = "model\\s*="
    )

    # Check for non-allowed estimators (anything except MLR and ML)
    if (grepl("estimator\\s*=", runcommand, ignore.case = TRUE)) {
      if (!grepl("estimator\\s*=\\s*['\"]?(MLR|ML)['\"]?", runcommand, ignore.case = TRUE)) {
        invalid_patterns$estimator <- "estimator\\s*="
      }
    }

    # Check for non-allowed missing data methods (anything except FIML)
    if (grepl("missing\\s*=", runcommand, ignore.case = TRUE)) {
      if (!grepl("missing\\s*=\\s*['\"]?FIML['\"]?", runcommand, ignore.case = TRUE)) {
        invalid_patterns$missing <- "missing\\s*="
      }
    }

    conflicts <- character(0)
    for (name in names(invalid_patterns)) {
      if (grepl(invalid_patterns[[name]], runcommand, ignore.case = TRUE)) {
        conflicts <- c(conflicts, name)
      }
    }

    if (length(conflicts) > 0) {
      warning("'runcommand' contains conflicting lavaan arguments: ",
        paste(conflicts, collapse = ", "), ". ",
        "This may cause unexpected behavior.",
        call. = FALSE
      )
    }
  }

  p <- ncol(data)
  N <- nrow(data)
  pst <- p * (p + 1) / 2

  # fit the saturated model in lavaan
  S1.mod <- write_sat(varnames = colnames(data))

  S1 <- try(eval(parse(text = paste(
    "sem(S1.mod, data = data, missing = 'ml', ",
    runcommand, ")"
  ))), silent = TRUE)
  # fixed the line below, need to test
  if (!inherits(S1, "try-error")) {
    if (inspect(S1, "converged") == TRUE && is.null(vcov(S1)) == FALSE) {
      shb <- fitted.values(S1)$cov # sigma-hat-beta
      mhb <- fitted.values(S1)$mean # mu-hat-beta
      ohb <- vcov(S1) # asy cov matrix, could be robust to nonnormality
      N <- S1@Data@nobs[[1]]

      # Add convergence info to output
      stage1_info <- list(
        converged = lavInspect(S1, "converged"),
        iterations = lavInspect(S1, "iterations")
      )

      S1.output <- list(shb, mhb, ohb, N, stage1_info)
    } else {
      S1.output <- NULL
    } # end if converged loop
  } else {
    S1.output <- NULL
  } # end try-error loop
  return(S1.output)
}

#' stage1a
#'
#' Stage1a of TSML: Converts the means, covariance matrix, and asymptotic covariance matrix
#' of components to the corresponding quantities for the composites
#'
#' @param S1.output The output of stage1, a list with 4 objects (N, means, covariance matrix,
#' and asymptotic covariance matrix of components)
#' @param C The matrix of component-composite assignments that is the output of stage0
#' (or user-supplied)
#'
#' @return  A list with 4 objects: mhd (estimated means),
#' shd (estimated covariance matrix), ohd (estimated asymptotic covariance matrix
#' of mhb and shb elements) for the composites, and N (sample size)
#'
#' @export
#'
#' @examples
#'
#' # an example using the first 18 variables in the simulated dataset misdata_mcar20
#' # reduce model size
#'
#' library(lavaan)
#' misdata1 <- misdata_mcar20[, 1:18]
#'
#' # composite model
#' mod1 <- "
#'  F1 =~ C1 + C2 + C3
#'  F2 =~ C4 + C5 + C6
#'  F2 ~ F1
#'  F2 ~~ F2
#'  F1 ~~ F1"

#' #manual computation for C (stage0) to avoid user input
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
#' out_s1<-stage1(misdata1)
#' out_s1a<-stage1a(out_s1,C)
#'
stage1a <- function(S1.output, C) {
  if (is.null(S1.output)) {
    stop("stage1 output is NULL")
  }

  # input validation
  validate_C_matrix(C)

  if (is.null(S1.output)) {
    S1a.output <- NULL
  } else {
    shb <- S1.output[[1]]
    mhb <- S1.output[[2]]
    ohb <- S1.output[[3]]
    N <- S1.output[[4]] # not used here, passed on to stage2
    stage1_info <- S1.output[[5]] # not used here, passed on to stage2

    cnames <- rownames(C)
    k <- nrow(C) # number of composites
    p <- ncol(C) # number of components
    pst <- p * (p + 1) / 2
    kst <- k * (k + 1) / 2

    # Cbig, covs in original order, the means
    # update: first means, then covs, to match NACOV order
    Cb <- matrix(0, (kst + k), (pst + p))
    # Cb[(kst + 1):(kst + k), (pst + 1):(pst + p)] <- C
    Cb[1:k, 1:p] <- C # means first
    Cbpart1 <- lav_matrix_duplication_ginv_pre(C %x% C) # new line 1
    # Cb[1:kst, 1:pst] <- lav_matrix_duplication_post(Cbpart1) # new line 2
    Cb[(k + 1):(kst + k), (p + 1):(pst + p)] <- lav_matrix_duplication_post(Cbpart1) # new line 2


    # saturated model parameters for the model based on the k composites:
    mhd <- C %*% mhb # mu-hat-delta
    rownames(mhd) <- cnames
    shd <- C %*% shb %*% t(C) # sigma-hat-delta
    rownames(shd) <- colnames(shd) <- cnames
    # dh <- c(lav_matrix_vech(shd),mhd) 		  #delta-hat
    ohd <- Cb %*% ohb %*% t(Cb) # ohm-hat-delta
    rownames(ohd) <- colnames(ohd) <- namesohd(cnames)
    # order in ohd: vectorized non-red Sigma, then mu
  } # end of big non-null else
  S1a.output <- list(shd, mhd, ohd, N, stage1_info)
  return(S1a.output)
}

#' stage2
#'
#' Performs stage2 of TSML. Fits the model to the estimated vector of means and the estimated
#' covariance matrix for the composites using complete data routines, adjusts naive
#' standard errors to obtain robust TS standard errors (for normal data), computes
#' the residual-based test statistic (for normal data)
#'
#' @param S1a.output Output from stage1a, a list with four objects (shd, mhd, ohd, N)
#' @param compmodel The lavaan model for the composites
#' @param runcommand2 Additional arguments to pass to lavaan
#' @param lavaan_function Which lavaan function to use to fit the model
#' @return An object of class `lavaan`
#' @export
#'
#' @examples
#'
#' # an example using the first 18 variables in the simulated dataset misdata_mcar20
#' # reduce model size
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

#' #manual computation for C (stage0) to avoid user input
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
#' out_s1 <- stage1(misdata1)
#' out_s1a <- stage1a(out_s1,C)
#' out_s2 <- stage2(out_s1a, compmodel = mod1, runcommand2="mimic='EQS'")

stage2 <- function(S1a.output, compmodel, runcommand2 = NULL,
                   lavaan_function = c("sem", "lavaan", "cfa", "growth", "sam")) {
  if (is.null(S1a.output)) {
    stop("stage1a output is NULL")
  }

  # Validate compmodel
  validate_compmodel(compmodel)

  # Validate runcommand2 argument
  if (!is.null(runcommand2)) {
    if (!is.character(runcommand2) || length(runcommand2) != 1) {
      stop("'runcommand2' must be a single character string or NULL", call. = FALSE)
    }

    # Check for potentially problematic specifications:
    # User cannot supply these
    # As they are automatically supplied from stage1
    invalid_patterns <- list(
      data = "data\\s*=",
      model = "model\\s*=",
      sample.cov = "sample\\.cov\\s*=",
      sample.mean = "sample\\.mean\\s*=",
      sample.nobs = "sample\\.nobs\\s*=",
      meanstructure = "meanstructure\\s*=",
      fixed.x = "fixed.x\\s*="
    )

    conflicts <- character(0)
    for (name in names(invalid_patterns)) {
      if (grepl(invalid_patterns[[name]], runcommand2, ignore.case = TRUE)) {
        conflicts <- c(conflicts, name)
      }
    }

    if (length(conflicts) > 0) {
      stop("'runcommand2' contains conflicting lavaan arguments: ",
        paste(conflicts, collapse = ", "), ". ",
        "These arguments are set by the stage2 function.",
        call. = FALSE
      )
    }
  }

  # Validate estimator if specified in runcommand2
  if (!is.null(runcommand2) && grepl("estimator\\s*=", runcommand2, ignore.case = TRUE)) {
    if (!grepl("estimator\\s*=\\s*['\"]?ML['\"]?", runcommand2, ignore.case = TRUE)) {
      stop("stage2 only supports estimator='ML'", call. = FALSE)
    }
  }

  shd <- S1a.output[[1]]
  mhd <- S1a.output[[2]]
  ohd <- S1a.output[[3]]
  N <- S1a.output[[4]]
  my_nacov <- N * ohd

  lavaan_function <- match.arg(lavaan_function)

  # JULY 2025:
  # ADDED NACOV and estimator = "MLM"
  # to robustify SEs and get Tres and fit indices automatically
  # Does not save "naive" SEs
  # "MLM" can refer to normal theory, depending on whether ohd
  # was computed using FIML or MLR -- this is determined by stage1, not stage2

  # to pass the old test
  if (!is.null(runcommand2) && grepl("mimic\\s*=\\s*['\"]EQS['\"]", runcommand2, ignore.case = TRUE)) {
    my_nacov <- my_nacov * (N) / (N - 1)
  }

  lavaan_call_string <- paste0(
    lavaan_function,
    "(model = compmodel, sample.cov = shd,
    sample.mean = mhd, sample.nobs = N,
    NACOV=my_nacov, estimator='MLM',
    test = c('browne.residual.adf','satorra.bentler'),
    fixed.x=FALSE, meanstructure=TRUE, ", runcommand2, ")"
  )

  S2 <- tryCatch(eval(parse(text = lavaan_call_string)), error = function(e) {
    message("lavaan error encountered in stage2: ", e$message)
    return(NA)
  })

  # may still need to catch cases where vcov has some diagonal NAs
  # as the robust SEs are now automatic, this is just for Tres
  # (or could save "naive" SEs, if later needed)
  # if (lavInspect(S2, "converged")) {
  #   vcov_result <- suppressWarnings(lavInspect(S2, "vcov"))
  #   if (is.matrix(vcov_result)) {
  #     ddh <- lavInspect(S2, "delta") # model derivatives
  #     bread <- lavInspect(S2, "vcov") * N
  #     Hh <- lavInspect(S2, "h1.information.expected")
  #     meat <- Hh %*% ddh
  #     Ohtt <- bread %*% t(meat) %*% ohd %*% meat %*% bread # ohm-hat-theta-tilde
  #
  #     # Residual-based test statistic
  #     # Should only be used when estimator="FIML" in Stage1?
  #     # Also, doesn't lavaan already have it?
  #     et <- c(residuals(S2)$mean, lav_matrix_vech(residuals(S2)$cov)) # so swap
  #     ahd <- solve(ohd.reordered) / N
  #     # Using N instead of N-1 to be consistent with lavaan
  #     Tres <- (N) * t(et) %*% (ahd - (ahd %*% ddh) %*% solve(t(ddh) %*% ahd %*% ddh) %*% (t(ddh) %*% ahd)) %*% et
  #     pval <- 1 - stats::pchisq(Tres, df = inspect(S2, "fit")["df"])

  # WE DO NOT NEED CLASS TWOSTAGE ANYMORE (for now)
  # S2 <- new("twostage", S2, twostage = list()) # change class to twostage

  # update se_ts for free parameters
  # set TS SEs to initially equal naive SEs, to match nonfree values and length
  # S2@twostage$se_ts <- S2@ParTable$se
  # set ses for free parameters to new TS SEs
  # NACOV update: suppress robustification (now automatic)
  # S2@twostage$se_ts[S2@ParTable$free != 0] <- sqrt(diag(Ohtt))


  # new test statistic code:
  # S2@twostage$test <- Tres
  # S2@twostage$df <- S2@Fit@test$standard$df
  # S2@twostage$pval <- pval

  # conv and iter info from Stage 1, for show:
  # stage1_info <- S1a.output[[5]]
  # S2@twostage$stage1_info <- stage1_info
  # } # end if vcov
  # } # end if

  return(S2)
}


#' twostage
#'
#' Fits the model to composite based on the raw data on components using item-level two-stage
#' maximum likelihood (TSML) of Savalei &amp Rhemtulla (2017). Adjusts naive
#' standard errors to obtain robust TS standard errors (for normal data), computes
#' the residual-based test statistic (for normal data).
#'
#  This function runs stage0, stage1, stage1a, stage2 in sequence.

#' @param data Data file for the components (items)
#' @param compmodel The lavaan model for the composites
#' @param C (optional) The matrix of component-composite assignments and weights (if NULL, user input will be requested to construct it)
#' @param which_col (optional) A vector of length of names(data), with entries
#' identifying the number of the composite, from which C will be created (assumes sums)
#' @param runcommand Additional arguments to pass to lavaan for stage1
#' @param runcommand2 Additional arguments to pass to lavaan for stage2
#'
#' @return An object of class `twostage`, inheriting class `lavaan`
#'
#'
#' @export
#'
#' @examples
#'
#' # Example 1: An example using the first 18 variables in the simulated
#' # dataset misdata_mcar20 with 20% missing data on about half the variables
#' # C1 - C6 are parcels formed using three variables each (in order)
#'
#' library(lavaan)
#' misdata1 <- misdata_mcar20[, 1:18]
#'
#' # composite model
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
#' out_ts <- twostage(data = misdata1, compmodel = mod1,C = C)
#'
#' #alternative specification (faster but more error-prone)
#' #each number indicates which component each composite belongs to
#' #in the same order as lavNames(mod1)
#' which_col <- c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6)
#' out_ts <- twostage(data = misdata1, compmodel = mod1,which_col = which_col)
#'
#' #Example2: TSML on a complete dataset tpbdata, with lavaan options set to match complete data ML
#' #TPB Model for Composites (Full mediation)
#' tpbmod<-'
#' INTALL ~ ATTALL + PBCALL + NORMALL
#' BEH ~ INTALL'
#'
#' #manual definition of C (instead of stage0 to avoid user input)
#' cnames<-lavNames(tpbmod)
#' C <- matrix(0,nrow=length(cnames),ncol=length(colnames(tpbdata)))
#' colnames(C)<-colnames(tpbdata)
#' rownames(C)<-cnames
#' C[1,c("INT1","INT2","INT3")]<-1
#' C[2,c("BEH")]<-1
#' C[3,grep("AT", names(tpbdata))]<-1
#' C[4,c("PBC1","PBC2","PBC3")]<-1
#' C[5,c("NORS1","NORS2","NORS3")]<-1
#'
#' out_ts <- twostage(data = tpbdata, compmodel = tpbmod,C = C,
#' runcommand = "information='expected'", runcommand2 = "
#' sample.cov.rescale=FALSE")
#' #The naive and TS standard errors should be identical
#'
#'
#' @references
#' Savalei, V., & Rhemtulla, M. (2017). Normal theory two-stage ML estimator when data are missing at the item level. Journal of Educational and Behavioral Statistics, 42(1), 1-27. https://doi.org/10.3102/1076998617694880


twostage <- function(data, compmodel, C = NULL, which_col = NULL,
                     runcommand = NULL, runcommand2 = NULL) {
  # stage 0, if needed: relating components to composite via user input
  if (is.null(C)) {
    C <- if (is.null(which_col)) {
      stage0(data = data, compmodel = compmodel)
    } else {
      stage0(data = data, compmodel = compmodel, which_col = which_col, type = 1)
    }
  }

  # Validate inputs
  validate_compmodel(compmodel)
  validate_C_matrix(C, data, compmodel)

  # stage 1: saturated model on components
  s1 <- stage1(data = data, runcommand = runcommand)
  # stage1a: saturated solution transformation to composites
  s1a <- stage1a(S1.output = s1, C = C)
  # stage2: compmodel fit to composites
  s2 <- stage2(S1a.output = s1a, compmodel = compmodel, runcommand2 = runcommand2)

  return(s2)
}
