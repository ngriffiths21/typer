# typer

## Overview

The typer package makes it easy to catch errors related to mismatched types. These kinds of errors can be especially difficult to track down because R is not a strongly typed language and many packages throw obscure errors when passed something unexpected.

Specifically, the package provides a function to scan a file's Roxygen2 comments for any type definitions, and use those to check whether types are ever used inconsistently.

## Installation

```r
devtools::install_github("ngriffiths21/typer")
```

## Usage

```r
typer::get_type_errors("yourscript.R")
```

## License

[MIT](https://choosealicense.com/licenses/mit/)
