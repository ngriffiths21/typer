match_call <- function (def, call) {
  stopifnot(is.call(call))
  match.call(definition = def, call = call, expand.dots = FALSE)
}

#' @title Check a File
#' @param filename File to check
#'
#' @export
get_type_errors <- function (filename) {
  typedefs <- get_file_typedefs(filename)
  validate_types(typedefs)
}

validate_types <- function (typedefs) {
  result <- lapply(typedefs, function (x) {
    validate_type(extract_fn_body(x$call), x$params, typedefs)
  })
  unlist(result)
}

extract_fn_body <- function (call) {
  call[[3]][[3]][[-1]]
}

validate_type <- function (expra, paramtypes, alltypes) {
  if (rlang::is_syntactic_literal(expra)) {
    "valid literal"
  } else if (is.call(expra)) {
    validate_call(expra, paramtypes, alltypes)
  } else {
    "can't tell"
  }
}

validate_call <- function (call, paramtypes, alltypes) {
  append(
    call_is_correct(
      map(call, ~ make_literal(., alltypes)),
      paramtypes,
      alltypes
    ),
    map(call[-1], ~ validate_type(., paramtypes, alltypes))
  )
}

make_literal <- function (expra, allexprs) {
  if(is.call(expra)) {
    returnval <- allexprs[[expra[[1]]]]$return
    if (!is.null(returnval)) {
      rlang::exec(returnval) # e.g. "logical" --> logical(). R is a truly stupid language
    }
  } else {
    expra
  }
}

call_is_correct <- function (literalcall, paramtypes, alltypes) {
  literalcall <- as.call(literalcall)
  knownfun <- alltypes[[rlang::as_string(literalcall[[1]])]]

  if (!is.null(knownfun)) {
    expandedcall <- match_call(eval(knownfun$call), literalcall)
    paramvals <- map_chr(knownfun$params, ~ typeof(expandedcall[[.$name]]))
    types <- map_chr(knownfun$params, ~ .$type)
    ifelse(types == paramvals, "ok", "error")
  }
}
