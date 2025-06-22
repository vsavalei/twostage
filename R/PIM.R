# Functions to create a lavaan model syntax for PIM (Pseudo-Indicator Model)
# With Observed Composites
# test_active_file("tests/testthat/test-PIM.R")
# TODO: ADD VARIANCES OR MARKER APPROACH FOR EXOGENEOUS LATENT VARS


# grab names of "latent" variables or components (i.e., not in dataset)
# from the model
composites <- function(C) {
  # no input validation, trust caller
  component_names <- colnames(C) # observed variables' names
  model_names <- rownames(C) # composite model variable names
  composites <- setdiff(model_names, component_names) # only var names that aren't in data
  common_elements <- intersect(model_names, component_names) # model var names that are also in the data

  if (length(common_elements) > 0) {
    message2 <- paste("The following observed variables will be in the model directly and not as part of any composite:", paste(common_elements, collapse = ", "))
    message(message2)
  }
  if (length(composites) == 0) {
    warning("No composite variables detected. All model variables correspond to observed variables. ", "Are you sure this is intended?", call. = FALSE)
  }

  return(composites)
}


# creates the first part of the PIM model syntax
# so this still created a latent variable with name BEH, not good
# need to select rows of C that are not excluded by composites function
# the syntax below will accommodate single component composites as well
# (i.e., if the user for some reason wants to rename an observed variable)
# TODO: Eventually, allow user to specify which indicator is the pseudo-indicator
#       for each composite
# TODO: Permit average and not just sum scores

PIM.uni <- function(C) {
  PIM.uni <- NULL
  cnames <- composites(C) # model var names that aren't in the data

  if (length(cnames) == 0) {
    return("")
  }

  C1 <- C[rownames(C) %in% cnames, ] # submatrix with rows that will be set up as "latent" composites
  for (j in seq_along(cnames)) {
    cnamesj <- colnames(C1)[C1[j, ] == 1]
    compj <- rownames(C1)[j]
    if (length(cnamesj) != 1) {
      allbut1 <- paste(sprintf("(-1)*%s", cnamesj[-1]), collapse = " + ")
      PIMj <- sprintf(
        "%s =~ 1*%s\n  %s ~ %s\n   %s ~~ 0*%s \n %s ~ 1  \n %s ~ 0*1",
        compj, cnamesj[1], cnamesj[1], allbut1, cnamesj[1], cnamesj[1],
        compj, cnamesj[1]
      )
    } else {
      PIMj <- sprintf(
        "%s =~ 1*%s\n  %s ~~ 0*%s \n %s ~ 1  \n %s ~ 0*1",
        compj, cnamesj[1], cnamesj[1], cnamesj[1],
        compj, cnamesj[1]
      )
    }
    compj_var <- paste(compj, "~~", compj) # composite variance (for "lavaan")

    PIM.uni <- paste(c(PIM.uni, PIMj, compj_var), collapse = "\n")
  }

  return(PIM.uni)
}

# RENAME -- very confusing
# additional syntax needed to fit the PIM model using lavaan rather than sem command
# explicit (residual) variances and intercepts
PIM.uni.lav <- function(C) {
  PIM.uni.lav <- NULL
  varnames <- rownames(C) # model var names, broader than cnames

  for (j in seq_along(varnames)) {
    varnamesj <- colnames(C)[C[j, ] == 1]
    compj <- rownames(C)[j]

    if (length(varnamesj) == 1) {
      # Check if this is an observed variable (composite name = component name)
      if (compj %in% colnames(C)) {
        # Observed variable: add free parameters
        varnamesjj <- varnamesj
      } else {
        # True single-component composite: no additional parameters
        varnamesjj <- character(0)
      }
    } else {
      varnamesjj <- varnamesj[-1]
    }

    # Only create syntax if there are variables to process
    if (length(varnamesjj) > 0) {
      # freely estimated variance for each indicator but the first
      varsj <- paste(paste(varnamesjj, "~~", varnamesjj), collapse = "\n")
      # freely estimated intercept for each indicator but the first
      intsj <- paste(paste(varnamesjj, "~", 1), collapse = " \n ")

      PIM.uni.lav <- paste(c(PIM.uni.lav, varsj, intsj), collapse = "\n")
    }
  }
  return(PIM.uni.lav)
}



PIM.multi <- function(C) {
  allbut1 <- NULL # a vector
  cnames <- rownames(C)

  for (j in seq_along(cnames)) { # Use seq_along for safety
    cnamesj <- colnames(C)[C[j, ] == 1]
    if (length(cnamesj) > 1) { # Only add if there are multiple components
      allbut1j <- cnamesj[-1]
      allbut1 <- c(allbut1, allbut1j)
    }
  }

  # Check if allbut1 is empty or has fewer than 2 elements
  if (length(allbut1) < 2) {
    return(NULL) # or return("") depending on how it's used downstream
  }

  combs <- utils::combn(allbut1, 2, simplify = FALSE) # Now safe
  texts <- sapply(combs, function(x) paste(x, collapse = " ~~ "))
  PIM.multi1 <- paste(texts, collapse = "\n")

  # create a text object with covs among allbut1 elements and each composite:
  right_side <- paste(allbut1, collapse = "+")
  int1 <- paste(cnames, right_side, sep = " ~~ ")
  PIM.multi2 <- paste(int1, collapse = "\n")

  PIM.multi <- paste(c(PIM.multi1, PIM.multi2), collapse = "\n")
  return(PIM.multi)
}

## Explicitly correlates exogenous variables in a model for composites
## This includes observed and latent variables
## In the PIM.uni.lav, we already free variances of composites, so this
## will double up the syntax if applied indiscriminately.
## In the PIM.uni.lav, however, we do not have info about the model and
## names of true latent vars. Maybe combine these two functions later.
## Problem with message4: if user adds orthogonal covs to compmodel, it will still
## get printed. However, that message is no longer accurate, because user specification
## will override this duplicate specification. (verified). We need a better unified way to set up the entire PIM.
comp_exog_covs_syntax <- function(compmodel) {
  partable <- lavaanify(compmodel)

  # Find endogenous vars (values in lhs with ~ in op)
  end_names1 <- unique(partable$lhs[partable$op == "~"]) # do not need to worry about 1
  # Find more endogenous vars (values in rhs with =~ in op), indicators of factors
  end_names2 <- unique(partable$rhs[partable$op == "=~"])

  # Endogenous appear in lhs(to avoid 1?) but not those in end_names1 or end_names2
  # What about those that via fixed.x=TRUE would never appear in lhs, but only rhs?
  # Do we just ignore fixed.x=TRUE because of missing data?
  # "" excludes empty rhs, which corresponds to mean structure parameters
  # why isn't 1 stored in rhs? i don't know.
  exog_names <- unique(partable$rhs[!(partable$rhs %in% c(end_names1, end_names2, ""))])

  # below needs to work when there is only one exogenous var

  if (length(exog_names) < 2) {
    exog_covs <- "\n"
  } else {
    # Generate combinations of all pairs
    pairs <- utils::combn(exog_names, 2, simplify = FALSE)
    # Format pairs into the desired string
    fpairs <- sapply(pairs, function(x) paste(x[1], "~~", x[2]))

    # Collapse into a single string separated by newlines
    exog_covs <- paste(fpairs, collapse = "\n")
    message4 <- paste0(
      "The following exogeneous variables in the composites model will be  correlated by default:", paste(exog_names, collapse = ", "), ". \nIf you do not want this, modify the composite model syntax manually or set exog_cov=FALSE."
    )
    message(message4)
  }
  return(exog_covs)
}






#' Creates lavaan syntax for a Pseudo-Indicator Model
#'
#' Creates lavaan syntax for a Pseudo-Indicator Model (PIM) of Rose et al.
#' (2019).
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are
#'   components
#' @param compmodel A string with the lavaan model for composites
#' @param exog_cov Should exogenous variables in the composite model be correlated?
#' @returns A string with the full PIM model syntax
#' @export
#'
#' @details The names of the composite variables in `compmodel` should match the
#'  row names of the `C` matrix. This function does not check for this, as it
#'  just creates a text object and does not evaluate if `lavaan` can run it.
#'
#'
#' @examples
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
#' PIM_syntax(compmodel, C)
#'
#' @references Rose, N., Wagner, W., Mayer, A., & Nagengast, B. (2019).
#' Model-based manifest and latent composite scores in structural equation
#' models. Collabra: Psychology, 5(1), Article 9.
#' https://doi.org/10.1525/collabra.143
#'

PIM_syntax <- function(compmodel, C = NULL, exog_cov = TRUE) {
  if (is.null(C)) {
    stop("No C matrix provided; use stage0 function to create it from data and compmodel.")
  }

  # validate inputs
  validate_C_matrix(C)
  validate_compmodel(compmodel)

  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  PIMulav <- PIM.uni.lav(C)

  if (exog_cov) {
    comp_exog_covs_syntax <- comp_exog_covs_syntax(compmodel)
  } else {
    comp_exog_covs_syntax <- NULL
  }

  out <- paste(c(
    "##--------PIM setup (item-level): ----------##",
    PIMu, PIMm, PIMulav,
    "##--------END OF PIM setup (item-level)----------##",
    "",
    "##--------Composite Model (inspect carefully): ----------##",
    compmodel, comp_exog_covs_syntax
  ), collapse = "\n")


  return(out)
}



# Baseline Model Syntax for the Composites Model, internal
compmodel_base <- function(compmodel, exog_cov = TRUE) {
  comp_names <- lavNames(compmodel)

  # we need to exclude true latent variables
  partable <- lavaanify(compmodel)
  fnames <- unique(partable$lhs[partable$op == "=~"])
  obscomp_names <- setdiff(comp_names, fnames)

  part1 <- paste(sapply(obscomp_names, function(x) paste(x, "~~", x)),
    collapse = " \n "
  ) # all vars
  part2 <- paste(sapply(obscomp_names, function(x) paste(x, "~", 1)),
    collapse = " \n "
  ) # all means

  part3 <- NULL
  if (exog_cov) { # covs of exogenous variables
    # Find endogenous vars (values in lhs with ~ in op)
    end_names1 <- unique(partable$lhs[partable$op == "~"])
    # Find more endogenous vars (values in rhs with =~ in op), indicators of factors
    end_names2 <- unique(partable$rhs[partable$op == "=~"])
    exog_names <- unique(partable$rhs[!(partable$rhs %in% c(end_names1, end_names2, ""))])
    obsexog_names <- setdiff(exog_names, fnames) # those that are not factors

    if (length(obsexog_names) > 0) {
      # Generate combinations of all pairs
      pairs <- utils::combn(obsexog_names, 2, simplify = FALSE)
      # Format pairs into the desired string
      fpairs <- sapply(pairs, function(x) paste(x[1], "~~", x[2]))

      # Collapse into a single string separated by newlines
      part3 <- paste(fpairs, collapse = "\n")
    }
  }
  compmodel_base <- paste(c(part1, part2, part3), collapse = "\n")
  return(compmodel_base)
}

#' Baseline (Null) Model Syntax for the PIM
#'
#' Creates syntax for a special baseline (null) PIM model to compute incremental fit indices. The item-level model is the same. Uncorrelates all variables in the composite model while keeping the PIM structure at the item level.
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are
#'   components
#' @param compmodel A string with lavaan model for composites
#' @param exog_cov Should the exogenous variables (latent or observed) in the compmodel remain correlated in the baseline (null) model?
#' @returns A string with the PIM baseline model lavaan syntax
#' @export
#'
#' @examples
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
#' PIM_model_base <- PIM_syntax_base(compmodel = compmodel, C = C)
#'
PIM_syntax_base <- function(compmodel, C = NULL, exog_cov = TRUE) {
  # validate inputs
  validate_C_matrix(C)
  validate_compmodel(compmodel)

  # building syntax through smaller functions
  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  PIMulav <- PIM.uni.lav(C)

  basemodel <- compmodel_base(compmodel, exog_cov = exog_cov)

  out <- paste(c(
    "##--------PIM setup (item-level): ----------##",
    PIMu, PIMm, PIMulav,
    "##--------END OF PIM setup (item-level)----------##",
    "",
    "##--------Composite Model (inspect carefully): ----------##",
    basemodel
  ), collapse = "\n")
  return(out)
}

# fit saturated model for composites
# rewrite this whole function file so that all PIM functions
# are only called once, and all three models are created?
#' Title
#'
#' @param C A matrix of 0s and 1s, where rows are composites and columns are
#'   components
#' @param compmodel Composite model lavaan syntax
#'
#' @returns A string with the full PIM baseline model lavaan syntax
#' @export
#'
#' @examples
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
#' PIM_model_sat <- PIM_syntax_sat(C = C, compmodel = compmodel)
#'
PIM_syntax_sat <- function(compmodel, C = NULL) {
  compmodel_sat <- write_sat(model = compmodel)
  PIMu <- PIM.uni(C)
  PIMm <- PIM.multi(C)
  PIMulav <- PIM.uni.lav(C)

  out <- paste(c(PIMu, PIMm, PIMulav, compmodel_sat), collapse = "\n")
  return(out)
}
