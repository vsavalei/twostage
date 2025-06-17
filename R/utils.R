# Helper functions for input validation

# Validate C matrix structure
validate_C_matrix <- function(C, data = NULL,compmodel = NULL) {

  if (!is.matrix(C)) {
    stop("C must be a matrix", call. = FALSE)
  }

  if (is.null(rownames(C))) {
    stop("C must have row names (composite variable names)", call. = FALSE)
  }

  if (is.null(colnames(C))) {
    stop("C must have column names (component variable names)", call. = FALSE)
  }

  #The following checks are used for now
  #However, in principle, data can contain variables not used, and this could be a warning
  if (!is.null(data) && ncol(C) != ncol(data)) {
    stop("Number of columns in C (", ncol(C), ") must equal number of columns in data (", ncol(data), ")", call. = FALSE)
  }

  if (!is.null(compmodel) && nrow(C) != length(lavaan::lavNames(compmodel))) {
    stop("Number of rows in C (", nrow(C), ") must equal number of
         observed variable names in compmodel", call. = FALSE)
  }

  invisible(NULL)
}


# Validate compmodel syntax

validate_compmodel <- function(compmodel) {
  if (missing(compmodel) || is.null(compmodel)) {
    stop("'compmodel' argument is required and cannot be NULL", call. = FALSE)
  }

  if (!is.character(compmodel) || length(compmodel) != 1) {
    stop("'compmodel' must be a single character string containing lavaan syntax",
         call. = FALSE)
  }

  invisible(NULL)
}
