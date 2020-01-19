match_call <- function (call) {
  stopifnot(is.call(call))
  fun <- eval(call[[1]])
  match.call(call, definition = fun, expand.dots = FALSE)
}

get_type_errors <- function (filename) {
  typedefs <- get_file_typedefs(filename)
  lapply(typedefs, validate_type)
}

validate_type <- function (typedef) {
  if (rlang::is_syntactic_literal(typedef$call)) {
    return(TRUE)
  } else if (is.call(typedef$call)) {
    typedef$call <- lapply(typedef$call, make_literal)
    call_is_correct(typedef)
  }
}

make_literal <- function (expra) {
  if (rlang::is_syntactic_literal(expra)) {
    return(expra)
  } else if (is.call(expra)) {
    stop("don't know how to look up the type definitions yet!")
    lookup_ret_type(expra[[1]]) # check if there is a known return type
  }
}

call_is_correct <- function (typedef) {
  stop("don't know how to see if the actual passed parameters match the type definition")
}
