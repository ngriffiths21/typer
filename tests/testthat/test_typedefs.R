context("Parsing typedefs")
test_that("It can get typedefs from an example file", {
  expect_type(get_file_typedefs("../example.R"), "list")
})

test_that("It matches literals to types in add_test", {
  expect_equal(get_type_errors("../example.R")[["add_test.x"]], "ok")
})

test_that("It matches return values to types in add_test", {
  expect_equal(get_type_errors("../example.R")[["add_test.x"]], "ok")
})

test_that("It matches parameters to types in add_eight", {
  expect_equal(get_type_errors("../example.R")[["add_eight.x"]], "ok")
})

test_that("It detects an error in add_wrong", {
  expect_equal(get_type_errors("../example.R")[["add_wrong.x"]], "error")
})


test_that("It detects mismatched parameters", {
  expect_equal(get_type_errors("../example.R")[["add_wrongparam.x"]], "error")
})

test_that("It matches types of variables in the environment", {
  expect_true(FALSE)
})
