context("Parsing typedefs")

test_file_errs <- get_type_errors("../example.R")

test_that("It can get typedefs from an example file", {
  expect_type(get_file_typedefs("../example.R"), "list")
})

test_that("It matches literals to types in add_test", {
  expect_true(test_file_errs[[12]]$passed)
})

test_that("It matches return values to types in add_test", {
  expect_true(test_file_errs[[7]]$passed)
})

test_that("It matches parameters to types in add_eight", {
  expect_true(test_file_errs[[11]]$passed)
})

test_that("It detects an error in add_wrong", {
  expect_false(test_file_errs[[15]]$passed)
})


test_that("It detects mismatched parameters", {
  expect_false(test_file_errs[[17]]$passed)
})
