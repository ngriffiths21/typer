context("Parsing typedefs")
test_that("It can get typedefs from an example file", {
  expect_type(get_file_typedefs("../example.R"), "list")
})

test_that("It matches literals to types in add_test", {
  expect_equal(get_type_errors("../example.R")[["add_test2"]], "ok")
})

test_that("It matches return values to types in add_test", {
  expect_equal(get_type_errors("../example.R")[["add_test1"]], "ok")
})

test_that("It matches parameters to types in add_eight", {
  expect_equal(get_type_errors("../example.R")[["add_eight1"]], "ok")
})

test_that("It detects an error in add_wrong", {
  expect_equal(get_type_errors("../example.R")[["add_wrong1"]], "error")
})
