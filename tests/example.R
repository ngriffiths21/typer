#! addstrings :: character -> character
addstrings <- function(...) {
  c(...)
}

#! add_six :: numeric -> numeric
add_six <- function(x) {
  x + 6
}

#! add_eight :: numeric -> numeric
add_eight <- function(x) {
  add_six(x) + 2
}
