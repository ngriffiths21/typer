match_call <- function (def, call) {
  stopifnot(is.call(call))
  match.call(definition = def, call = call, expand.dots = FALSE)
}

#' @title Check a File
#' @param filename File to check
#' @export
get_type_errors <- function (filename) {
  typedefs <- get_file_typedefs(filename)
  validate_types(typedefs)
}

validate_types <- function (typedefs) {
  result <- lapply(typedefs, function (x) {
    validate_type(x$call[[3]][[3]], x$params, typedefs)
  })
  unlist(result)
}

validate_type <- function (call, paramtypes, alltypes) {
  if (rlang::is_syntactic_literal(call)) {
    return(TRUE)
  } else if (is.call(call)) {
    listres <- lapply(call[-1], function (x) { validate_type(x, paramtypes, alltypes) })
    literalcall <- lapply(call, function (x) { make_literal(x, alltypes) })
    result <- call_is_correct(literalcall, paramtypes, alltypes)
    return(append(listres, result))
  } else {
    NA
  }
}

make_literal <- function (expra, allexprs) {
  if (rlang::is_syntactic_literal(expra)) {
    return(expra)
  } else if (is.call(expra)) {
    returnval <- allexprs[[expra[[1]]]]$return
    if (!is.null(returnval)) {
      rlang::exec(returnval) # hacking the fact that types have constructors of same name
    }
  } else if (is.symbol(expra)) {
    return(expra)
  } else {
    NA
  }
}

call_is_correct <- function (literalcall, paramtypes, alltypes) {
  literalcall <- as.call(literalcall)
  knownfun <- alltypes[[rlang::as_string(literalcall[[1]])]]

  if (!is.null(knownfun)) {
    expandedcall <- match_call(eval(knownfun$call), literalcall)
    result <- lapply(knownfun$params, function (x) {
      if (!is.null(expandedcall[[x$name]])) {
        if (x$type != typeof(expandedcall[[x$name]])) {
          message(
            "types don't match!\n",
            "expected:", x$type, "\ngot:", typeof(expandedcall[[x$name]]),
            "\nin function:", knownfun$alias
          )
          return("error")
        } else {
          return("ok")
        }
      }
    })
    return(result)
  }
}
