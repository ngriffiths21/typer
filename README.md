# TypeR

### Type definitions list

Type definitions parsed from Roxygen2 comment blocks are represented as a named list. The format is the following:

```r
List of 1
$ function_name: List of 4
 ..$ params: List of 1
   ..$ `...`: List of 3
      ..$ name: chr "..."
	  ..$ type: chr "list"
 ..$ return: chr "character"
 ..$ call  : language {function def}
 ..$ alias : chr "function_name"
```
