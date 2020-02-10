#' @param x <integer> a number
#' @return <integer> a number
addtwo <- function (x) {
  x + 2L
}

#' @param x <integer> a number
#' @return <integer a number
good <- function (x) {
  addtwo(x)
}

#' @param x <character> a char
#' @return <integer> a number
bad <- function (x) {
  addtwo(x)
}
