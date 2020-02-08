get_file_typedefs <- function(filename) {
  blocks <- roxygen2::parse_file(filename)
  names(blocks) <- lapply(blocks, function (x) { x$object$alias })

  blocks <- lapply(blocks, function (block) {
    block$tags <- parse_tags(block$tags)
    block
  })

  typedefs_from_blocks(blocks)
}

isReturn <- function (x) {
  map_lgl(x, ~.$tag == "return")
}

isParam <- function (x) {
  map_lgl(x, ~.$tag == "param")
}

typedefs_from_blocks <- function(blocks) {
  lapply(
    blocks,
    function (block) {
      list(
        params = flatten(map(block$tags[isParam(block$tags)], construct_param)),
        return = block$tags[isReturn(block$tags)][[1]]$val$type,
        call = block$call,
        alias = block$object$alias
      )
    })
}

construct_param <- function(tg) {
  type <- tg$val$type
  name <- tg$val$name

  rlang::list2(!!name := type)
}

parse_tags <- function(tags) {
  lapply(tags, parse_type)
}

parse_type <- function(tag) {
  brackets <- regex("(?<=<).*(?=>)")
  switch(
    tag$tag,
    return = tag$val <- list(
      type = str_extract(tag$val, brackets),
      description = tag$val
    ),
    param = tag$val$type <- str_extract(tag$val$description, brackets)
  )
  tag
}
