match_call <- function (call) {
  stopifnot(is.call(call))
  fun <- eval(call[[1]])
  match.call(call, definition = fun, expand.dots = FALSE)
}

get_type_errors <- function (filename) {
  typedefs <- get_file_typedefs(filename)
  validate_types(typedefs)
}

validate_types <- function (typedefs) {
  lapply(typedefs, function (x) {
    validate_type(x$call[[3]][[3]], x$params, typedefs)
  })
}

validate_type <- function (call, paramtypes, alltypes) {
  if (rlang::is_syntactic_literal(call)) {
    return(TRUE)
  } else if (is.call(call)) {
    lapply(call[-1], function (x) { validate_type(x, paramtypes, alltypes) })
    literalcall <- lapply(call, function (x) { make_literal(x, alltypes) })
    call_is_correct(literalcall, paramtypes)
  } else {
    NA
  }
}

make_literal <- function (expra, allexprs) {
  if (rlang::is_syntactic_literal(expra)) {
    return(expra)
  } else if (is.call(expra)) {
    allexprs[[expra[[1]]]]$return
  } else if (is.symbol(expra)) {
    return(expra)
  } else {
    NA
  }
}

call_is_correct <- function (literalcall, paramtypes) {
  message("call tree:")
  message(str(literalcall))
  message("type expected:")
  message(paramtypes)
  
  warning("don't know how to check if expected type matches actual")
  "no errors I know of"
}
