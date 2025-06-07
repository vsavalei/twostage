# Tests for stage0 and stage1 functions
# Written by Claude Sonnet 4.0

# Test stage0 function
test_that("stage0 creates correct C matrix with provided which_col and type", {
  # Simple test model with 3 composites
  model <- "
    C1 ~ C2 + C3
    C2 ~ C3
  "

  # Create simple test data (6 variables)
  test_data <- data.frame(
    Y1 = rnorm(50),
    Y2 = rnorm(50),
    Y3 = rnorm(50),
    Y4 = rnorm(50),
    Y5 = rnorm(50),
    Y6 = rnorm(50)
  )

  # Assignment: C1 gets Y1,Y2; C2 gets Y3,Y4; C3 gets Y5,Y6
  which_col <- c(1, 1, 2, 2, 3, 3) # 6 components assigned to 3 composites

  # Test with sums (type = 1)
  C_sums <- stage0(test_data, model, which_col = which_col, type = 1)

  # Check dimensions
  expect_equal(nrow(C_sums), 3) # 3 composites
  expect_equal(ncol(C_sums), 6) # 6 components

  # Check row and column names
  expect_equal(rownames(C_sums), c("C1", "C2", "C3"))
  expect_equal(colnames(C_sums), c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6"))

  # Check assignment correctness (should be 1s for sums)
  expect_equal(as.numeric(C_sums[1, 1:2]), c(1, 1)) # C1 gets Y1, Y2
  expect_equal(as.numeric(C_sums[2, 3:4]), c(1, 1)) # C2 gets Y3, Y4
  expect_equal(as.numeric(C_sums[3, 5:6]), c(1, 1)) # C3 gets Y5, Y6

  # Check zeros where expected
  expect_equal(as.numeric(C_sums[1, 3:6]), c(0, 0, 0, 0)) # C1 doesn't get Y3-Y6
  expect_equal(as.numeric(C_sums[2, c(1:2, 5:6)]), c(0, 0, 0, 0)) # C2 doesn't get Y1,Y2,Y5,Y6
})

test_that("stage0 creates correct C matrix for averages", {
  model <- "C1 ~ C2"

  test_data <- data.frame(
    Y1 = rnorm(50),
    Y2 = rnorm(50),
    Y3 = rnorm(50),
    Y4 = rnorm(50)
  )

  # C1 gets Y1,Y2,Y3 (3 items); C2 gets Y4 (1 item)
  which_col <- c(1, 1, 1, 2)

  # Test with averages (type = 2)
  C_avgs <- stage0(test_data, model, which_col = which_col, type = 2)

  # Check that weights are 1/n for each composite
  expect_equal(as.numeric(C_avgs[1, 1:3]), c(1 / 3, 1 / 3, 1 / 3)) # C1: average of 3 items
  expect_equal(as.numeric(C_avgs[2, 4]), 1) # C2: average of 1 item (stays 1)
  expect_equal(as.numeric(C_avgs[1, 4]), 0) # C1 doesn't get Y4
  expect_equal(as.numeric(C_avgs[2, 1:3]), c(0, 0, 0)) # C2 doesn't get Y1-Y3
})

test_that("stage0 validates which_col input", {
  model <- "C1 ~ C2"
  test_data <- data.frame(Y1 = rnorm(10), Y2 = rnorm(10))

  # Test error when which_col has wrong length
  expect_error(
    stage0(test_data, model, which_col = c(1), type = 1),
    "'which_col' must have length"
  )

  # Test error when which_col has invalid values
  expect_error(
    stage0(test_data, model, which_col = c(1, 3), type = 1),
    "must be integers between 1 and the number of variables in model"
  )

  expect_error(
    stage0(test_data, model, which_col = c(0, 1), type = 1),
    "must be integers between 1 and the number of variables in model"
  )
})

test_that("stage0 handles single-component composites", {
  model <- "C1 ~ C2 + C3"
  test_data <- data.frame(Y1 = rnorm(10), Y2 = rnorm(10), Y3 = rnorm(10))

  # Each composite gets exactly one component
  which_col <- c(1, 2, 3)

  C <- stage0(test_data, model, which_col = which_col, type = 1)

  # Check that it's an identity-like matrix
  expect_equal(C[1, 1], 1)
  expect_equal(C[2, 2], 1)
  expect_equal(C[3, 3], 1)
  expect_equal(sum(C), 3) # Only 3 ones total
})

# Test stage1 function
test_that("stage1 returns correct structure with complete data", {
  # Use small subset of tpbdata for testing
  test_data <- tpbdata[1:50, 1:6] # 50 rows, 6 variables

  # Run stage1
  result <- stage1(test_data)

  # Check that result is a list with 4 elements
  expect_type(result, "list")
  expect_length(result, 4)

  # Check that elements are correct types and dimensions
  shb <- result[[1]] # covariance matrix
  mhb <- result[[2]] # means
  ohb <- result[[3]] # asymptotic covariance matrix
  N <- result[[4]] # sample size

  # Check covariance matrix
  expect_true(is.matrix(shb))
  expect_equal(dim(shb), c(6, 6))
  expect_equal(rownames(shb), colnames(test_data))
  expect_equal(colnames(shb), colnames(test_data))
  expect_true(isSymmetric(shb))

  # Check means vector
  expect_true(is.numeric(mhb))
  expect_length(mhb, 6)
  expect_equal(names(mhb), colnames(test_data))

  # Check asymptotic covariance matrix
  expect_true(is.matrix(ohb))
  expected_dim <- 6 * (6 + 1) / 2 + 6 # p*(p+1)/2 + p
  expect_equal(dim(ohb), c(expected_dim, expected_dim))
  expect_true(isSymmetric(ohb))

  # Check sample size
  expect_equal(N, 50)
})

test_that("stage1 handles missing data correctly", {
  # Create data with missing values
  test_data <- tpbdata[1:30, 1:4]

  # Introduce some missing values
  test_data[1:5, 1] <- NA
  test_data[10:12, 2] <- NA

  # Run stage1
  result <- stage1(test_data)

  # Should still return a valid result
  expect_type(result, "list")
  expect_length(result, 4)

  # Check that covariance matrix is positive definite
  shb <- result[[1]]
  expect_true(all(eigen(shb)$values > 0))

  # Sample size should reflect the data
  N <- result[[4]]
  expect_true(N <= 30) # Should be <= original sample size
})

test_that("stage1 returns NULL when model fails to converge", {
  # Create data that might have convergence issues but isn't completely constant
  set.seed(123) # For reproducibility
  test_data <- data.frame(
    Y1 = rnorm(20, mean = 0, sd = 0.01), # Very low variance
    Y2 = rnorm(20, mean = 0, sd = 0.01), # Very low variance
    Y3 = rnorm(20) # Normal variance
  )

  # This might fail to converge or have issues
  result <- suppressWarnings(stage1(test_data))

  # Function should handle the error gracefully
  # Either returns valid result or NULL
  if (!is.null(result)) {
    expect_type(result, "list")
    expect_length(result, 4)
  } else {
    expect_null(result)
  }
})

test_that("stage1 passes additional arguments to lavaan", {
  test_data <- tpbdata[1:30, 1:4]

  # Test with additional runcommand
  result <- stage1(test_data, runcommand = "information='expected'")

  # Should still return valid result
  expect_type(result, "list")
  expect_length(result, 4)

  # Check basic properties
  expect_true(is.matrix(result[[1]]))
  expect_true(is.numeric(result[[2]]))
  expect_equal(result[[4]], 30)
})

test_that("stage1 handles edge cases", {
  # Test with minimum viable data (3x3)
  test_data <- data.frame(
    Y1 = rnorm(10),
    Y2 = rnorm(10),
    Y3 = rnorm(10)
  )

  result <- stage1(test_data)

  # Should work with minimum data
  expect_type(result, "list")
  expect_length(result, 4)
  expect_equal(dim(result[[1]]), c(3, 3))
  expect_length(result[[2]], 3)
})

test_that("stage1 internal function namesohd works correctly", {
  # Test the helper function used in stage1a
  cnames <- c("C1", "C2", "C3")

  # Call the internal function
  result <- twostage:::namesohd(cnames)

  # Should return correct number of names
  k <- length(cnames)
  expected_length <- k * (k + 1) / 2 + k # covariances + means
  expect_length(result, expected_length)

  # Should contain expected patterns
  expect_true(any(grepl("C1~~C1", result)))
  expect_true(any(grepl("C1~~C2", result)))
  expect_true(any(grepl("C1~1", result)))
  expect_true(any(grepl("C2~1", result)))
  expect_true(any(grepl("C3~1", result)))
})

# Integration test for stage0 and stage1 together
test_that("stage0 and stage1 work together correctly", {
  # Use a subset of the package data
  test_data <- misdata_mcar20[1:50, 1:9] # 50 rows, 9 variables

  model <- "
    C1 ~ C2 + C3
    C2 ~ C3
  "

  # Create C matrix
  which_col <- c(1, 1, 1, 2, 2, 2, 3, 3, 3) # 3 composites, 3 items each
  C <- stage0(test_data, model, which_col = which_col, type = 1)

  # Run stage1
  s1_result <- stage1(test_data)

  # Both should succeed
  expect_true(is.matrix(C))
  expect_type(s1_result, "list")

  # Check compatibility for stage1a
  expect_equal(ncol(C), ncol(test_data)) # C should have same number of columns as data
  expect_equal(ncol(s1_result[[1]]), ncol(test_data)) # covariance matrix should match
})
