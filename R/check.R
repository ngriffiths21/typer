match_call <- function (def, call) {
  stopifnot(is.call(call))
  match.call(definition = def, call = call, expand.dots = FALSE)
}

#' Check a file for type errors
#'
#' Check for inconsistent use of types within one file.
#' 
#' This function checks for inconsistent uses
#' of types within a file. It checks all function calls in
#' the file to see whether arguments in the form
#' of literals, parameters, or return values of other calls
#' match the type the argument is documented to be.
#'
#' The return value is a list of errors, with each error reported
#' in the following format: `error$fn`, the name of the function that was
#' given the wrong argument; `error$call`: full call object that produced
#' the error; `error$type`, which is "literal", "parameter", or "returnval";
#' `error$expected`, the expected type, as given by `typeof()`; and
#' `error$got`, the received type, as given by `typeof()`.
#' 
#' @param filename File to check
#' @return A list containing the errors
#'
#' @export
get_type_errors <- function (filename) {
  typedefs <- get_file_typedefs(filename)
  validate_types(typedefs)
}

validate_types <- function (typedefs) {
  result <- map(typedefs, function (x) {
    validate_type(extract_fn_body(x$call), x$params, typedefs, x$alias)
  })
  result <- flatten(result)
  result
}

extract_fn_body <- function (call) {
  call[[3]][[3]][[-1]]
}

validate_type <- function (expra, paramtypes, alltypes, currfun) {
  if (rlang::is_syntactic_literal(expra)) {
    list(type_response(message, str_c("valid literal `", expra, "`"), fn = currfun))
  } else if (is.call(expra)) {
    validate_call(expra, paramtypes, alltypes, currfun)
  } else if (is.symbol(expra)) {
    list(type_response(message, str_c("valid symbol `", expra, "`"), fn = currfun))
  } else {
    list(type_response(warning, str_c("can't tell how to check `", expra, "`"), fn = currfun))
  }
}

validate_call <- function (call, paramtypes, alltypes, currfun) {
  append(
    list(
      call_is_correct(
        append(
          call[[1]], # must pluck element to reconstruct a call
          map(call[-1], ~ make_literal(., alltypes, paramtypes))
        ),
        paramtypes,
        alltypes
      )
    ),
    flatten(map(call[-1], ~ validate_type(., paramtypes, alltypes, currfun)))
  )
}

make_literal <- function (expra, allexprs, paramtypes) {
  if (is.call(expra)) {
    returnval <- allexprs[[expra[[1]]]]$return
    if (!is.null(returnval)) {
      rlang::exec(returnval) # e.g. "logical" --> logical(). R is a truly stupid language
    }
  } else if (is.symbol(expra)) {
    returnval <- paramtypes[[rlang::as_string(expra)]]
    if (!is.null(returnval)) {
      rlang::exec(returnval)
    } else {
      warning("Typer cannot check the type of variable `", expra, "`. Consider using parameters.")
      return(expra)
    }
  } else {
    if (!rlang::is_syntactic_literal(expra)) {
      warning("Typer cannot determine how to check expression `", expra, "`.")
    }
    expra
  }
}

call_is_correct <- function (literalcall, paramtypes, alltypes) {
  literalcall <- as.call(literalcall)
  knownfun <- alltypes[[rlang::as_string(literalcall[[1]])]]
  strfun <- as.list(literalcall)[[1]]

  if (!is.null(knownfun)) {
    expandedcall <- match_call(eval(knownfun$call), literalcall)
    paramvals <- imap_chr(knownfun$params, ~ typeof(expandedcall[[.y]]))
    types <- map_chr(knownfun$params, ~ .)

    if(types == paramvals) {
      type_response(message, str_c("call to `", strfun, "` is correct"),
                    list(fn = strfun, type = "call", call = expandedcall, types = types, passed = TRUE))
    } else {
      type_response(collect_error, str_c("error in call to `", strfun, "`!", " wanted ", types, "; got ", paramvals),
                    list(fn = strfun, type = "call", call = expandedcall, types = types, passed = FALSE))
    }
  } else {
    type_response(warning, str_c("cannot check function `", strfun, "` because it is not defined."), strfun)
  }
}

collect_error <- function (text) {
  warning(text)
}

type_response <- function(reporter, text, comparison = NULL, fn = NULL) {
  do.call(reporter, list(text))
  if (!is.null(comparison)) {
    return(comparison)
  }
  list(fn = fn, type = "message", text = text)
}
