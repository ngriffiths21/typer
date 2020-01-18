get_file_typedefs <- function(filename) {
  blocks <- roxygen2::parse_file(filename)
  blocks <- lapply(blocks, function (block) {
    block$tags <- parse_tags(block$tags)
    block
  })

  typedefs_from_blocks(blocks)
}

isVar <- function (x) {
  map_lgl(x, ~.$tag %in% c("param", "return"))
}

typedefs_from_blocks <- function(blocks) {
  lapply(
    blocks,
    function (block) {
      result <- map(block$tags[isVar(block$tags)], construct_typedef)
      result$call <- block$call
      result$alias <- block$object$alias
      result
    })
}

construct_typedef <- function(tg) {
  tag <- tg$tag
  type <- tg$val$type
  name <- tg$val$name

  list(tag = tag, name = name, type = type)
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
