#' @export
parse_commented_lines <- function(text) {
  ifelse(is.character(text), text, stop("Not a string."))

  text %>%
    str_split(regex("\\n")) %>%
    unlist() %>%
    str_extract("#!.*") %>%
    na.omit() %>%
    as.list() %>%
    map(~unlist(str_split(., regex("[:whitespace:]+"))))
}

check_file <- function(file) {
  text <- read_file(file)
  types <- parse_type_defs(parse_commented_lines(text))
  fndefs <- map(parse(file), parse_for_fns)

  map(types, ~check_def(., types, fndefs))
}

check_def <- function(x, types, fndefs) {
  warning("check_def is incomplete. It should check that functions called by current_fn treat arguments as having the same types.")
  current_fn <- fndefs[[x$name]] # fndefs needs to be subsettable by name for this to work
  check_num_args(x, current_fn)
}

parse_type_defs <- function(clines) {
  map(clines, function (x) {
    list(name = x[[2]], args = list(x[[4]]), val = x[[6]])
  })
}

parse_for_fns <- function(expra) {
  if (rlang::as_string(expra[[1]]) == "<-") {
    if (!is.call(expra[[3]])) return(NA)
    if (rlang::as_string(expra[[3]][[1]]) == "function") {
      return(list(
        name = rlang::as_string(expra[[2]]),
        args = expra[[3]][[2]],
        def = expra[[3]][[3]]
      ))
    }
  }
}

check_num_args <- function(types, defobject) {
  if (length(defobject$args) != length(types$args))
    stop("argument lengths don't match!")
}
