#' @param ... <character>
#' @return <character>
addstrings <- function(...) {
  c(...)
}

#' @param x <integer> A number
#' @return <integer> The number plus six
add_six <- function(x) {
  x + 6
}

#' @param x <integer> A number
#' @return <integer> The number plus eight
add_eight <- function(x) {
  add_six(x) + 2
}

#' @param x <integer> A num
#' @return <integer> A num
add_test <- function (x) {
  add_eight(add_six(3L)) + x
}

#' @param x <integer> A num
#' @return <integer> Another num
add_wrong <- function (x) {
  add_test("bad idea")
}
