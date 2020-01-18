context("Parsing typedefs")
test_that("It can get typedefs from an example file", {
  expect_type(get_file_typedefs("../example.R"), "list")
})

test_that("It can match typedefs for errors", {
  expect_type(get_type_errors("../example.R"), "list")
})
