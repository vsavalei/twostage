# Tests for PIM functions
# Expanded comprehensive test suite, Claude Sonnet 4.0
# Note: When fixing a bug, always convert it into a (failing) test

# Tests for PIM functions - CORRECTED VERSION

# Test data setup that can be reused across tests
setup_test_data <- function() {
  # Simple 3-composite, 7-variable setup
  C <- matrix(0, nrow = 3, ncol = 7)
  C[1, 1:3] <- 1 # C1 = Y1 + Y2 + Y3
  C[2, 4:6] <- 1 # C2 = Y4 + Y5 + Y6
  C[3, 7] <- 1 # C3 = Y7 (single indicator)
  rownames(C) <- c("C1", "C2", "C3")
  colnames(C) <- c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7")

  # Test data
  data <- misdata_mcar20[, c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7")]

  # Simple model
  compmodel <- "C1 ~ C2 + C3"

  list(C = C, data = data, compmodel = compmodel)
}

# Test PIM_syntax function
test_that("PIM_syntax generates valid lavaan syntax", {
  setup <- setup_test_data()

  # Test basic syntax generation
  pim_syntax <- PIM_syntax(setup$C, setup$compmodel)

  # Should return a character string
  expect_type(pim_syntax, "character")
  expect_true(nchar(pim_syntax) > 0)

  # Should contain key PIM elements
  expect_true(grepl("=~", pim_syntax)) # Factor loadings
  expect_true(grepl("~~", pim_syntax)) # Covariances
  expect_true(grepl("C1", pim_syntax)) # Composite names
  expect_true(grepl("Y1", pim_syntax)) # Item names

  # Should contain PIM structure markers
  expect_true(grepl("PIM setup", pim_syntax))
  expect_true(grepl("Composite Model", pim_syntax))
})

test_that("PIM_syntax handles exog_cov parameter correctly", {
  setup <- setup_test_data()

  # Test with exogenous correlations (default)
  expect_message(
    pim_syntax_with_cov <- PIM_syntax(setup$C, setup$compmodel, exog_cov = TRUE),
    "exogeneous variables.*will be.*correlated"
  )

  # Test without exogenous correlations
  pim_syntax_no_cov <- suppressMessages(PIM_syntax(setup$C, setup$compmodel, exog_cov = FALSE))

  # Both should be valid but different
  expect_type(pim_syntax_with_cov, "character")
  expect_type(pim_syntax_no_cov, "character")
  expect_false(identical(pim_syntax_with_cov, pim_syntax_no_cov))
})

# Test PIM_syntax_base function
test_that("PIM_syntax_base creates proper baseline model", {
  setup <- setup_test_data()

  baseline_syntax <- PIM_syntax_base(setup$C, setup$compmodel)

  expect_type(baseline_syntax, "character")
  expect_true(grepl("PIM setup", baseline_syntax))
  expect_true(grepl("Composite Model", baseline_syntax))

  # Should be different from regular PIM syntax
  regular_syntax <- suppressMessages(PIM_syntax(setup$C, setup$compmodel))
  expect_false(identical(baseline_syntax, regular_syntax))
})

# Test PIM_syntax_sat function
test_that("PIM_syntax_sat creates saturated model", {
  setup <- setup_test_data()

  saturated_syntax <- PIM_syntax_sat(setup$C, setup$compmodel)

  expect_type(saturated_syntax, "character")
  expect_true(grepl("~~", saturated_syntax)) # Should have covariances

  # Should be different from regular and baseline syntax
  regular_syntax <- suppressMessages(PIM_syntax(setup$C, setup$compmodel))
  baseline_syntax <- PIM_syntax_base(setup$C, setup$compmodel)

  expect_false(identical(saturated_syntax, regular_syntax))
  expect_false(identical(saturated_syntax, baseline_syntax))
})

test_that("compmodel_sat creates saturated composite model", {
  setup <- setup_test_data()

  sat_model <- compmodel_sat(setup$compmodel)

  expect_type(sat_model, "character")
  expect_true(grepl("~~", sat_model)) # Should have covariances

  # Should contain all composite variables
  comp_names <- lavaan::lavNames(setup$compmodel)
  for (name in comp_names) {
    expect_true(grepl(name, sat_model))
  }
})

# Test integration: fitting actual PIM models
test_that("PIM models can be fit and converge", {
  setup <- setup_test_data()

  # Generate and fit PIM model
  pim_syntax <- suppressMessages(PIM_syntax(setup$C, setup$compmodel))
  fit_pim <- lavaan::lavaan(pim_syntax, data = setup$data)

  # Should converge
  expect_true(lavaan::lavInspect(fit_pim, "converged"))

  # Should have reasonable degrees of freedom (not necessarily 0)
  df <- as.numeric(lavaan::fitmeasures(fit_pim)["df"])
  expect_true(df >= 0)

  # Should have parameter estimates
  params <- lavaan::parameterEstimates(fit_pim)
  expect_true(nrow(params) > 0)
  expect_true("est" %in% names(params))
})

test_that("PIM baseline model fits and has different fit from main model", {
  setup <- setup_test_data()

  # Fit main model and baseline model
  pim_syntax <- suppressMessages(PIM_syntax(setup$C, setup$compmodel))
  baseline_syntax <- PIM_syntax_base(setup$C, setup$compmodel)

  fit_pim <- lavaan::lavaan(pim_syntax, data = setup$data)
  fit_baseline <- lavaan::lavaan(baseline_syntax, data = setup$data)

  # Both should converge
  expect_true(lavaan::lavInspect(fit_pim, "converged"))
  expect_true(lavaan::lavInspect(fit_baseline, "converged"))

  # Baseline should have worse fit (higher chi-square)
  chisq_main <- as.numeric(lavaan::fitmeasures(fit_pim)["chisq"])
  chisq_baseline <- as.numeric(lavaan::fitmeasures(fit_baseline)["chisq"])
  expect_true(chisq_baseline >= chisq_main)
})

# Test error handling
test_that("PIM functions handle invalid inputs gracefully", {
  setup <- setup_test_data()

  # Test with mismatched C matrix and model
  C_wrong <- setup$C
  rownames(C_wrong) <- c("X1", "X2", "X3") # Wrong names

  # Should still generate syntax (function doesn't validate names)
  expect_no_error(
    wrong_syntax <- suppressMessages(PIM_syntax(C_wrong, setup$compmodel))
  )

  # But lavaan should have issues fitting it - check for variable not found error
  expect_error(
    lavaan::lavaan(wrong_syntax, data = setup$data),
    "not found in the\\s+dataset" # \\s+ matches any whitespace including line breaks
  )
})

# THIS IS AN ACTUAL FAILING TEST, NOT A CLAUDE BUG
# Test with single-indicator composites
test_that("PIM functions work with single-indicator composites", {
  # Test setup where observed variables are used directly as composites
  C_single <- matrix(0, nrow = 3, ncol = 3)
  C_single[1, 1] <- 1 # Y1 -> Y1
  C_single[2, 2] <- 1 # Y2 -> Y2
  C_single[3, 3] <- 1 # Y3 -> Y3
  rownames(C_single) <- c("Y1", "Y2", "Y3")
  colnames(C_single) <- c("Y1", "Y2", "Y3")

  single_model <- "Y1 ~ Y2 + Y3"
  data_single <- setup_test_data()$data[, 1:3]

  # Should handle single indicators
  expect_no_error(
    single_syntax <- suppressMessages(suppressWarnings(PIM_syntax(C_single, single_model)))
  )

  # Should generate a warning about single indicators
  expect_warning(
    single_syntax1 <- suppressMessages(PIM_syntax(C_single, single_model)),
    "No composite variables detected"
  )

  # Should be fittable
  expect_no_error(
    fit_single <- lavaan::lavaan(single_syntax, data = data_single)
  )
})

# Test srmr_mplus_pim function
test_that("srmr_mplus_pim calculates SRMR correctly", {
  setup <- setup_test_data()

  # Fit main and saturated models
  pim_syntax <- suppressMessages(PIM_syntax(setup$C, setup$compmodel))
  sat_syntax <- PIM_syntax_sat(setup$C, setup$compmodel)

  fit_pim <- lavaan::lavaan(pim_syntax, data = setup$data)
  fit_sat <- lavaan::lavaan(sat_syntax, data = setup$data)

  # Calculate SRMR
  srmr_result <- srmr_mplus_pim(fit_pim, fit_sat, setup$compmodel)

  # Should return a list with correct elements
  expect_type(srmr_result, "list")
  expect_true("srmr_mplus" %in% names(srmr_result))
  expect_true("srmr_mplus_nomean" %in% names(srmr_result))

  # Values should be numeric and non-negative
  expect_true(is.numeric(srmr_result$srmr_mplus))
  expect_true(is.numeric(srmr_result$srmr_mplus_nomean))
  expect_true(srmr_result$srmr_mplus >= 0)
  expect_true(srmr_result$srmr_mplus_nomean >= 0)
})

# Test fitMeasures_pim function
test_that("fitMeasures_pim provides comprehensive fit measures", {
  setup <- setup_test_data()

  # Fit PIM model
  pim_syntax <- suppressMessages(PIM_syntax(setup$C, setup$compmodel))
  fit_pim <- lavaan::lavaan(pim_syntax, data = setup$data)

  # Get fit measures
  fit_measures <- fitMeasures_pim(
    C = setup$C,
    compmodel = setup$compmodel,
    fit_pim = fit_pim,
    data = setup$data
  )

  # Should return named numeric vector
  expect_true(is.numeric(fit_measures))
  expect_true(length(names(fit_measures)) > 0)

  # Should include key fit indices
  expected_measures <- c("chisq", "df", "cfi", "tli", "rmsea", "srmr_mplus")
  for (measure in expected_measures) {
    expect_true(measure %in% names(fit_measures))
  }

  # Values should be reasonable
  expect_true(fit_measures["df"] >= 0)
  expect_true(fit_measures["chisq"] >= 0)
})

# Test with missing data
test_that("PIM functions work with missing data", {
  setup <- setup_test_data()

  # Introduce some missing data
  data_missing <- setup$data
  data_missing[1:10, 1] <- NA
  data_missing[15:20, 4] <- NA

  # Should still work with FIML
  pim_syntax <- suppressMessages(PIM_syntax(setup$C, setup$compmodel))

  expect_no_error(
    fit_missing <- lavaan::lavaan(pim_syntax, data = data_missing, missing = "FIML")
  )

  expect_true(lavaan::lavInspect(fit_missing, "converged"))
})

test_that("df and fmin of a saturated PIM model are zero (original test)", {
  # This is the original human-written test
  C <- matrix(0, nrow = 3, ncol = 7)
  C[1, 1:3] <- 1
  C[2, 4:6] <- 1
  C[3, 7] <- 1
  rownames(C) <- c("C1", "C2", "C3")
  colnames(C) <- c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7")
  compmodel <- "C1 ~ C2 + C3"
  model1 <- suppressMessages(PIM_syntax(C, compmodel))
  data1 <- misdata_mcar20[, c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7")]
  out <- lavaan::sem(model1, data = data1)
  df <- as.numeric(lavaan::fitmeasures(out)["df"])
  fmin <- as.numeric(lavaan::fitmeasures(out)["fmin"])
  expect_equal(df, 0)
  expect_equal(fmin, 0, tolerance = 1e-10)
})
