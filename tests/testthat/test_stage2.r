# tests for stage2
test_that("stage2 returns correct residual-based test statistic for mod1 example", {
  # Setup from the example
  library(lavaan)
  misdata1 <- misdata_mcar20[, 1:18]

  mod1 <- "
  F1 =~ C1 + C2 + C3
  F2 =~ C4 + C5 + C6
  F2 ~ F1
  F2 ~~ F2
  F1 ~~ F1"

  cnames <- lavNames(mod1)
  C <- matrix(0, nrow = length(cnames), ncol = length(colnames(misdata1)))
  colnames(C) <- colnames(misdata1)
  rownames(C) <- cnames
  C[1, 1:3] <- 1
  C[2, 4:6] <- 1
  C[3, 7:9] <- 1
  C[4, 10:12] <- 1
  C[5, 13:15] <- 1
  C[6, 16:18] <- 1

  out_s1 <- stage1(misdata1)
  out_s1a <- stage1a(out_s1, C)
  out_s2 <- stage2(out_s1a,
    compmodel = mod1
  )

  # testing twostage function, should be equivalent to out_s2
  out_ts <- twostage(misdata1, mod1, C)

  # testing twostage function with robust stage1
  out_ts_robust <- twostage(misdata1, mod1, C, runcommand = 'estimator="MLR"')


  # Test standard, SB, and residual-based TSML statistics
  # Test standard test statistic
  expect_equal(out_s2@Fit@test$standard$stat, 1.380, tolerance = 0.001)
  expect_equal(out_s2@Fit@test$standard$df, 8)
  expect_equal(out_s2@Fit@test$standard$stat, out_ts@Fit@test$standard$stat, tolerance = 0.001)

  # Test scaled (Satorra-Bentler) test statistic
  expect_equal(out_s2@Fit@test$satorra.bentler$stat, 1.072, tolerance = 0.001)
  expect_equal(out_s2@Fit@test$satorra.bentler$stat, out_ts@Fit@test$satorra.bentler$stat, tolerance = 0.001)

  # Test residual-based (ADF) test statistic
  expect_equal(out_s2@Fit@test$browne.residual.adf$stat, 1.047, tolerance = 0.001)
  expect_equal(out_s2@Fit@test$browne.residual.adf$stat, out_ts@Fit@test$browne.residual.adf$stat, tolerance = 0.001)

  # Test TS SEs and naive TS estimates
  expected_est <- c(1.00000000, 1.23206722, 1.43365721, 1.00000000, 1.15715256, 1.48240400, 0.49919443, 0.43463813, 0.59570238, 3.09184995, 3.39591882, 2.85353353, 3.70487064, 3.53297160, 2.76047596, 0.25669279, -0.13344278, 0.10067814, 0.09150368, 0.05108941, -0.05121546, 0.00000000, 0.00000000)

  expected_se_ts <- c(0.0000000, 0.4626183, 0.5516027, 0.0000000, 0.4988818, 0.6641133, 0.2658475, 0.3010257, 0.3274151, 0.4166768, 0.5113200, 0.5694187, 0.5116147, 0.5456375, 0.6383874, 0.1408314, 0.1505088, 0.1477008, 0.1559816, 0.1575152, 0.1529460, 0.0000000, 0.0000000)

  expected_se_ts_robust <- c(0.0000000, 0.4262963, 0.5288779, 0.0000000, 0.4733794, 0.6098921, 0.2617311, 0.3156603, 0.2953644, 0.4036646, 0.4813129, 0.5950842, 0.4349656, 0.5937066, 0.5611496, 0.1408862, 0.1506837, 0.1479144, 0.1541025, 0.1543490, 0.1515935, 0.0000000, 0.0000000)


  expect_equal(parameterestimates(out_s2)$se, expected_se_ts,
    tolerance = 0.00001
  )
  expect_equal(parameterestimates(out_s2)$est, expected_est,
    tolerance = 0.0001
  )

  expect_equal(parameterestimates(out_s2)$se,
    parameterestimates(out_ts)$se,
    tolerance = 0.00001
  )

  # robust should not influence estimates
  expect_equal(parameterestimates(out_ts)$est,
    parameterestimates(out_ts_robust)$est,
    tolerance = 0.00001
  )

  # robust ses
  expect_equal(expected_se_ts_robust,
    parameterestimates(out_ts_robust)$se,
    tolerance = 0.00001
  )
})



# Claude written (superficial, but keep)
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

# no longer as relevant
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

  # expect_s4_class(result, "twostage")
  expect_s4_class(result, "lavaan")

  # Should have converged
  expect_true(lavaan::lavInspect(result, "converged"))

  # Should have parameter estimates
  params <- lavaan::parameterEstimates(result)
  expect_true(nrow(params) > 0)
  expect_true("est" %in% names(params))

  # Should have TSML standard errors
  # expect_true("se_ts" %in% names(result@twostage))

  # Should have TSML test statistic
  # expect_true(!is.null(result@twostage$test))
  # expect_true(!is.null(result@twostage$df))
  # expect_true(!is.null(result@twostage$pval))
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
  expect_error(
    stage2(NULL, "C1 ~ C2"),
    "stage1a output is NULL"
  )
})
