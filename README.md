
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

str(addtwo(10L))
#>  int 12
```

Then run it through the type checker.

``` r
typer::get_type_errors(system.file("example", "simple.R", package = "typer"))
#>  int 12
#> Warning in do.call(reporter, list(text)): cannot check function `+` because it
#> is not defined.
#> valid symbol `x`
#> valid literal `2`
#> [[1]]
#> `+`
#> 
#> [[2]]
#> [[2]]$fn
#> [1] "addtwo"
#> 
#> [[2]]$type
#> [1] "message"
#> 
#> [[2]]$text
#> [1] "valid symbol `x`"
#> 
#> 
#> [[3]]
#> [[3]]$fn
#> [1] "addtwo"
#> 
#> [[3]]$type
#> [1] "message"
#> 
#> [[3]]$text
#> [1] "valid literal `2`"
```

## License

[MIT](https://choosealicense.com/licenses/mit/)
