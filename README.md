
# typer

## Overview

The typer package searches R scripts for type errors. These kinds of
errors can be especially difficult to track down because R is a weakly
typed language and many functions throw [confusing
errors](https://stackoverflow.com/questions/11308367/error-in-my-code-object-of-type-closure-is-not-subsettable)
when you pass them an unexpected type.

The `get_type_errors()` function opens an R script and searches for type
definitions, which are written using
[Roxygen2](http://r-pkgs.had.co.nz/man.html#roxygen-comments) style
comments. Then it checks whether types are used consistently throughout
the script.

## Installation

``` r
devtools::install_github("ngriffiths21/typer")
```

## Usage

First, write type annotations in Roxygen2 comments. Types should be
placed in angle brackets at the start of the description of the
parameter or return value. Use `class(x)` as the type for `x`.

``` r
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
```

Then run it through the type checker.

``` r
typer::get_type_errors(system.file("example", "simple.R", package = "typer"))
#> Warning in (function (text) : error in call to `addtwo` in `bad`! wanted
#> integer; got character
#> [[1]]
#> [[1]]$fn
#> [1] "bad"
#> 
#> [[1]]$type
#> [1] "call"
#> 
#> [[1]]$call
#> addtwo(x = character(0))
#> 
#> [[1]]$expected_params
#>         x 
#> "integer" 
#> 
#> [[1]]$passed
#> [1] FALSE
```

## License

[MIT](https://choosealicense.com/licenses/mit/)
