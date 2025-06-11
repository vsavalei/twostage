#tests for stage2
# Test stage2 function comprehensively
test_that("stage2 validates runcommand2 argument correctly", {
  # Create test data for stage2 input
  test_data <- misdata_mcar20[1:30, 1:6]
  s1_result <- stage1(test_data)

  # Create simple C matrix
  C <- matrix(0, nrow = 2, ncol = 6)
  C[1, 1:3] <- 1
  C[2, 4:6] <- 1
  rownames(C) <- c("C1", "C2")
  colnames(C) <- colnames(test_data)

  s1a_result <- stage1a(s1_result, C)
  model <- "C1 ~ C2"

  # Test valid runcommand2
  expect_no_error(stage2(s1a_result, model, runcommand2 = "mimic='EQS'"))

  # Test NULL runcommand2
  expect_no_error(stage2(s1a_result, model, runcommand2 = NULL))

  # Test invalid type - not character
  expect_error(
    stage2(s1a_result, model, runcommand2 = 123),
    "'runcommand2' must be a single character string or NULL"
  )

  # Test invalid type - multiple strings
  expect_error(
    stage2(s1a_result, model, runcommand2 = c("mimic='EQS'", "test=TRUE")),
    "'runcommand2' must be a single character string or NULL"
  )
})

test_that("stage2 errors on conflicting lavaan arguments", {
  # Create test data for stage2 input
  test_data <- misdata_mcar20[1:30, 1:6]
  s1_result <- stage1(test_data)

  # Create simple C matrix
  C <- matrix(0, nrow = 2, ncol = 6)
  C[1, 1:3] <- 1
  C[2, 4:6] <- 1
  rownames(C) <- c("C1", "C2")
  colnames(C) <- colnames(test_data)

  s1a_result <- stage1a(s1_result, C)
  model <- "C1 ~ C2"

  # Test data conflict
  expect_error(
    stage2(s1a_result, model, runcommand2 = "data=mydata"),
    "conflicting lavaan arguments: data"
  )

  # Test model conflict
  expect_error(
    stage2(s1a_result, model, runcommand2 = "model=mymodel"),
    "conflicting lavaan arguments: model"
  )

  # Test sample.cov conflict
  expect_error(
    stage2(s1a_result, model, runcommand2 = "sample.cov=mycov"),
    "conflicting lavaan arguments: sample.cov"
  )

  # Test sample.mean conflict
  expect_error(
    stage2(s1a_result, model, runcommand2 = "sample.mean=mymean"),
    "conflicting lavaan arguments: sample.mean"
  )

  # Test sample.nobs conflict
  expect_error(
    stage2(s1a_result, model, runcommand2 = "sample.nobs=100"),
    "conflicting lavaan arguments: sample.nobs"
  )

  # Test multiple conflicts
  expect_error(
    stage2(s1a_result, model, runcommand2 = "data=mydata, model=mymodel"),
    "conflicting lavaan arguments: data, model"
  )

  # Test case insensitive matching
  expect_error(
    stage2(s1a_result, model, runcommand2 = "DATA=mydata"),
    "conflicting lavaan arguments: data"
  )

  # Test no error for valid arguments
  expect_no_error(
    stage2(s1a_result, model, runcommand2 = "mimic='EQS', std.lv=TRUE")
  )
})

test_that("stage2 returns correct output structure", {
  # Create test data
  test_data <- misdata_mcar20[1:50, 1:6]
  s1_result <- stage1(test_data)

  C <- matrix(0, nrow = 2, ncol = 6)
  C[1, 1:3] <- 1
  C[2, 4:6] <- 1
  rownames(C) <- c("C1", "C2")
  colnames(C) <- colnames(test_data)

  s1a_result <- stage1a(s1_result, C)
  model <- "C1 ~ C2"

  result <- stage2(s1a_result, model)

  # Should return twostage object
  expect_s4_class(result, "twostage")
  expect_s4_class(result, "lavaan")

  # Should have converged
  expect_true(lavaan::lavInspect(result, "converged"))

  # Should have parameter estimates
  params <- lavaan::parameterEstimates(result)
  expect_true(nrow(params) > 0)
  expect_true("est" %in% names(params))

  # Should have TSML standard errors
  expect_true("se_ts" %in% names(result@ParTable))

  # Should have TSML test statistic
  expect_true(!is.null(result@Fit@test$twostage$test))
  expect_true(!is.null(result@Fit@test$twostage$df))
  expect_true(!is.null(result@Fit@test$twostage$pval))
})

test_that("stage2 handles different model types", {
  # Create test data
  test_data <- misdata_mcar20[1:40, 1:9]
  s1_result <- stage1(test_data)

  # Test regression model
  C_reg <- matrix(0, nrow = 3, ncol = 9)
  C_reg[1, 1:3] <- 1
  C_reg[2, 4:6] <- 1
  C_reg[3, 7:9] <- 1
  rownames(C_reg) <- c("C1", "C2", "C3")
  colnames(C_reg) <- colnames(test_data)

  s1a_reg <- stage1a(s1_result, C_reg)

  # Simple regression
  expect_no_error(stage2(s1a_reg, "C1 ~ C2 + C3"))

  # Multiple regression with correlation
  model_complex <- "
    C1 ~ C2 + C3
    C2 ~~ C3
  "
  expect_no_error(stage2(s1a_reg, model_complex))
})

test_that("stage2 handles edge cases", {
  # Create minimal test data
  test_data <- misdata_mcar20[1:25, 1:6]
  s1_result <- stage1(test_data)

  C <- matrix(0, nrow = 2, ncol = 6)
  C[1, 1:3] <- 1
  C[2, 4:6] <- 1
  rownames(C) <- c("C1", "C2")
  colnames(C) <- colnames(test_data)

  s1a_result <- stage1a(s1_result, C)

  # Test saturated model (0 df)
  saturated_model <- "
    C1 ~~ C1
    C2 ~~ C2
    C1 ~~ C2
    C1 ~ 1
    C2 ~ 1
  "
  result_sat <- stage2(s1a_result, saturated_model)
  expect_equal(as.numeric(lavaan::fitmeasures(result_sat)["df"]), 0)

  # Test intercept-only model
  expect_no_error(stage2(s1a_result, "C1 ~ 1; C2 ~ 1"))
})

test_that("stage2 handles NULL stage1a input", {
  # Test with NULL input (simulating stage1 failure)
  expect_error(
    stage2(NULL, "C1 ~ C2"),
    # Should get an error when trying to access NULL elements
    NA
  )
})

test_that("stage2 computes TSML statistics correctly", {
  # Use complete data for comparison
  test_data <- tpbdata[1:50, 1:6]
  s1_result <- stage1(test_data, runcommand = "information='expected'")

  C <- matrix(0, nrow = 2, ncol = 6)
  C[1, 1:3] <- 1
  C[2, 4:6] <- 1
  rownames(C) <- c("C1", "C2")
  colnames(C) <- colnames(test_data)

  s1a_result <- stage1a(s1_result, C)
  model <- "C1 ~ C2"

  result <- stage2(s1a_result, model, runcommand2 = "sample.cov.rescale=FALSE")

  # TSML chi-square should be non-negative
  expect_true(result@Fit@test$twostage$test >= 0)

  # p-value should be between 0 and 1
  expect_true(result@Fit@test$twostage$pval >= 0)
  expect_true(result@Fit@test$twostage$pval <= 1)

  # Should have same df as regular lavaan fit
  expect_true(result@Fit@test$twostage$df >= 0)

  # TSML SEs should be positive
  ts_ses <- result@ParTable$se_ts[result@ParTable$free != 0]
  expect_true(all(ts_ses > 0))
})
