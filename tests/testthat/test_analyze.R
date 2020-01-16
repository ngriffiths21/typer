context("testing parsing comments")
text <- glue::glue("hello
newline here
line with a #comment
#! function :: a -> a
#! anotherfun :: a -> b")

test_that("I can extract comments and do stuff", {
  result <- parse_commented_lines(text)
  
  expect_type(result, "list")
  expect_equal(result[[1]], c("#!", "function", "::", "a", "->", "a"))
  expect_equal(result[[2]], c("#!", "anotherfun", "::", "a", "->", "b"))
})

test_that("I can construct a definition object", {
  result <- parse_type_defs(parse_commented_lines(text))[[1]]

  expect_equal(result$name, "function")
  expect_equal(result$args[[1]], "a")
  expect_equal(result$val, "a")
})

test_that("I can check a file", {
  result <- check_file("../example.R")
  expect_true(result)
})
