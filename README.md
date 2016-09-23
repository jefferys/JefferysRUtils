
<!-- README.md is generated from README.Rmd. Please edit that file -->
JefferysRUtils
==============

A collection of utilities. These are functions that are commonly used in R packages I write or that solve some tiny problem in a generic way but are too small to release as their own package. Kind of a dumping ground, actually. I expect that over time functions may migrate out of here to other packages.

Currently there are three main groupings of functions:

-   String utilities
-   List utilities
-   Logging utilities for use with the [`futile.logger`](https://CRAN.R-project.org/package=futile.logger) package.

### String utilities

Most useful here are the character concatenation operators `%p%` and `%pp%` which act like paste0(x,y) and paste(x,y), respectively.

``` r
library("JefferysRUtils")
"Paste" %p% "No" %p% "Spaces."
#> [1] "PasteNoSpaces."
"Paste" %pp% "With" %pp% "Spaces."
#> [1] "Paste With Spaces."
"Paste" %p% "Mixed" %pp% "Spaces."
#> [1] "PasteMixed Spaces."
("Paste" %pp% "across" %pp% "line breaks."
   %pp% "Requires '()' if operator starts second line.")
#> [1] "Paste across line breaks. Requires '()' if operator starts second line."
( c( "Probably shouldn't", "I wouldn't" ) %pp% c( "use with", "apply to" )
   %pp% "vectors." %pp% "But it will work (like nested 'paste()')." )
#> [1] "Probably shouldn't use with vectors. But it will work (like nested 'paste()')."
#> [2] "I wouldn't apply to vectors. But it will work (like nested 'paste()')."
```

### Logging utilities

Functions are provided to support logging to the file and the screen but filtering at different levels. This may be supported directly by futile.logger at some point.

You have to initialize the file and screen loggers before use, then logging in tandem is provided with separate logging commands, one for each level, e.g. `sayInfo("Message")`.

### List utilities

Currently only provides a version of the base S3 function `merge` that works on lists.
