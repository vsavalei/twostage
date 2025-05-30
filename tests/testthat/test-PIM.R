# testing advice/general philosophy:

#
# Focus on testing the external interface to your functions - if you test the
# internal interface, then it’s harder to change the implementation in the
# future because as well as modifying the code, you’ll also need to update all
# the tests.
#
# Strive to test each behaviour in one and only one test. Then if that behaviour
# later changes you only need to update a single test.
#
# Avoid testing simple code that you’re confident will work. Instead focus your
# time on code that you’re not sure about, is fragile, or has complicated
# interdependencies. That said, we often find we make the most mistakes when we
# falsely assume that the problem is simple and doesn’t need any tests.
#
# Always write a test when you discover a bug. You may find it helpful to adopt
# the test-first philosophy. There you always start by writing the tests, and
# then write the code that makes them pass. This reflects an important problem
# solving strategy: start by establishing your success criteria, how you know if
# you’ve solved the problem.
#


# this test is for a scenario with three composites, one of length one
test_that("df and fmin of a PIM model are zero", {
  # C1 is the sum of Y1, Y2, and Y3
  #' #C2 is the sum of Y4, Y5, and Y6
  #' #C3 is Y7
  C <- matrix(0, nrow = 3, ncol = 7)
  C[1, 1:3] <- 1
  C[2, 4:6] <- 1
  C[3, 7] <- 1
  rownames(C) <- c("C1", "C2", "C3")
  colnames(C) <- c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7")
  compmodel <- "C1 ~ C2 + C3"
  model1 <- PIM_syntax(C, compmodel)
  data1 <- misdata_mcar20[, c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7")]
  out <- lavaan::sem(model1, data = data1)
  df <- as.numeric(fitmeasures(out)["df"])
  fmin <- as.numeric(fitmeasures(out)["fmin"])
  expect_equal(df, 0)
  expect_equal(fmin, 0)
})

# add a test where the expectation is an error
# expect_warning()
# expect_error()
# Does the code fail? Specifically, does it fail for the right reason?
# Does the accompanying message make sense to the human who needs to deal with the error?
# If you have the choice, express your expectation in terms of the condition’s
# class, instead of its message.

# expect_message()
# expect_no_error(1 / 2)

# add a test that the result is the same as with composites for complete data

# based on:
# devtools::test_coverage_active_file()
# cover message2
# i.e., add an example where composite is the same as component in name
# (df will fail here)
