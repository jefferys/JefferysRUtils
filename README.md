
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis-CI Build
Status](https://travis-ci.org/jefferys/JefferysRUtils.svg?branch=master)](https://travis-ci.org/jefferys/JefferysRUtils)

# JefferysRUtils

A collection of utilities. These are functions that are commonly used in
R packages I write or that solve some tiny problem in a generic way but
are too small to release as their own package. Kind of a dumping ground,
actually. I expect that over time functions may migrate out of here to
other packages.

Currently there are four main groupings of functions:

-   String utilities
-   List utilities
-   Logging utilities for use with the
    [`futile.logger`](https://CRAN.R-project.org/package=futile.logger)
    package
-   file utilities

### String utilities

Most useful here are the character concatenation operators `%p%` and
`%pp%` which act like paste0(x,y) and paste(x,y), respectively.

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

Functions are provided to support logging to the file and the screen but
filtering at different levels. This may be supported directly by
futile.logger at some point.

You have to initialize the file and screen loggers before use, then
logging in tandem is provided with separate logging commands, one for
each level, e.g. `sayInfo("Message")`.

### List utilities

Currently only provides a version of the base S3 function `merge` that
works on lists.

### File utilities

To allow applying a function to file in one step, two apply-like
functions are defined:

-   `fileLineApply` applies a supplied function to each line of a file
    or connection (read as text). Basically implements
    `sapply(readlines(file), FUN, ...)`
-   `fileBlockApply` applies a supplied function to a vector of lines
    from a file or connection. . Basically implements
    `FUN(readlines(file), ...)`

Both functions are implemented in a way that makes them work even on
large files, possibly files larger than would otherwise fit in memory.
Connections are supported so functions can be applied to compressed
files or files being read from URLs. Additionally, a `filter` flag is
provided to allow returning values selected by a logical function (or
for `fileBlockApply` an index returning function).

#### Unix-like file grep

For example, a simple file grep that returns lines from a (fake) file
connection:

``` r
content <- c( "One line", "Two lines.", "", "Four" )
con <- textConnection( content )
fileBlockApply( con, "grep", pattern="line", value=TRUE )
#> [1] "One line"   "Two lines."
```

#### Complex function applied line by line

Applying a function by line is often not necessary, may be simpler when
a function is complex and may be faster as only requires one file pass.
Here I have a function that converts a comma-separated string of
key=integer entries into a named vector. I could rewrite the function to
apply successive vector operations on a vector if inputs, but it easier
to just apply the function.

``` r
content <- c( "A=1,B =2, c = 3", "C=4", "B=1,A=", "" )
con <- textConnection( content )
parseKeyValues <- function(x, sep= "\\s*,\\s*", valSep= "\\s*=\\s*") {
   pairs <- strsplit(x, sep)[[1]]
   kv <- strsplit(pairs, valSep)
   values <- as.integer(sapply(kv, `[`, 2))
   names(values) <- toupper(sapply(kv, `[`, 1))
   return(values)
}
fileLineApply( con, "parseKeyValues" )
#> [[1]]
#> A B C 
#> 1 2 3 
#> 
#> [[2]]
#> C 
#> 4 
#> 
#> [[3]]
#>  B  A 
#>  1 NA 
#> 
#> [[4]]
#> named integer(0)
```

#### Files are read in chunks

To support large files, these file apply functions read in a file in
blocks of `chunkSize` lines, keeping only the results after processing a
chunk. E.g. this can work if only a few lines are being returned when
applying grep() to a very large file. However, the default is set so all
lines from a file are read in one chunk as most times files are small
enough. Setting some reasonable chunk size given available memory and
file line size is needed for large files.

Internally, the results from each block are stored as a list element and
are joined together only when results are returned. To get the raw list
of results split by block, set `unlist=FALSE`. Joining results together
is done with `unlist(recursive=FALSE)`, and this may do unexpected
things if your results are matrices or each line is a vector.

Note: Setting `chunkSize` to a small number is not something you should
do, but it is done in this example for expository purposes.

``` r
content <- c( "One line", "Two lines.", "", "Four" )
con <- textConnection( content )
fileBlockApply( con, function (x) { lengths(strsplit(x, "\\s+")) > 1 },
               chunkSize= 2 )
#> [1]  TRUE  TRUE FALSE FALSE

# Have to rewind the connection on each for each pass
con <- textConnection( content )
fileBlockApply( con, function (x) { lengths(strsplit(x, "\\s+")) > 1 },
               chunkSize= 2, unlist=FALSE )
#> [[1]]
#> [1] TRUE TRUE
#> 
#> [[2]]
#> [1] FALSE FALSE
```

If a function returns an index, it will be relative to the start of each
block, not the file.

``` r
# Returns index relative to chunk, if any match.
con <- textConnection( content )
fileBlockApply( con, "grep", pattern= "F" )
#> [1] 4

con <- textConnection( content )
fileBlockApply( con, "grep", pattern= "F", chunkSize= 2 )
#> [1] 2

con <- textConnection( content )
fileBlockApply( con, "grep", pattern= "F", chunkSize= 2, unlist= FALSE )
#> [[1]]
#> integer(0)
#> 
#> [[2]]
#> [1] 2
```

#### Selection with filtering functions

It is easy to select based on the result of a logical function by
setting `filter= TRUE`. `fileBlockApply` also supports selection by
index-returning functions. The block-relative offset is automatically
handled, whether or not you keep the block structure

``` r
content <- c( "One line", "Two lines.", "", "Four" )

# Logical function
con <- textConnection( content )
fileLineApply( con, function (x) { lengths(strsplit(x, "\\s+")) > 1 } )
#> [1]  TRUE  TRUE FALSE FALSE

# Filtering by logical function
con <- textConnection( content )
fileLineApply( con, function (x) { lengths(strsplit(x, "\\s+")) > 1 },
               filter= TRUE )
#> [1] "One line"   "Two lines."

# Indexes are relative to file chunks
con <- textConnection( content )
fileBlockApply( con, "grep", pattern= "F")
#> [1] 4

con <- textConnection( content )
fileBlockApply( con, "grep", pattern= "F", chunkSize= 2)
#> [1] 2

# Can filter by index regardless of file chunking (fileBlockApply only). 
con <- textConnection( content )
fileBlockApply( con, "grep", pattern= "F", filter= TRUE )
#> [1] "Four"

con <- textConnection( content )
fileBlockApply( con, "grep", pattern= "F", chunkSize= 2, filter= TRUE )
#> [1] "Four"
```

#### Problems with unlisting

If the `apply` of a function to a line results in neither a list nor a
single element vector, it is probably best to keep the block structure
and manually merge results. Unlisting destroys non-list sub-structures
like vectors or objects without remorse. It will interact especially
problematically with `fileLineApply`, which simplifies to arrays when
possible. Setting `.simplify=FALSE` will preserve per-line structure as
a list.

``` r
# Returns a vector for each line, simplifies into a matrix.
con <- textConnection( content )
fileLineApply( con, function (x) { c(nchar(x), 42) },
                 chunkSize= 2, unlist=FALSE )
#> [[1]]
#>      [,1] [,2]
#> [1,]    8   10
#> [2,]   42   42
#> 
#> [[2]]
#>      [,1] [,2]
#> [1,]    0    4
#> [2,]   42   42

# If don't simplify, get a list of list, one per line in each block
con <- textConnection( content )
fileLineApply( con, function (x) { c(nchar(x), 42) },
                 chunkSize= 2, unlist=FALSE, .simplify = FALSE )
#> [[1]]
#> [[1]][[1]]
#> [1]  8 42
#> 
#> [[1]][[2]]
#> [1] 10 42
#> 
#> 
#> [[2]]
#> [[2]][[1]]
#> [1]  0 42
#> 
#> [[2]][[2]]
#> [1]  4 42

# Unlisting only unlists one level of lists
con <- textConnection( content )
fileLineApply( con, function (x) { c(nchar(x), 42) },
                 chunkSize= 2, .simplify = FALSE )
#> [[1]]
#> [1]  8 42
#> 
#> [[2]]
#> [1] 10 42
#> 
#> [[3]]
#> [1]  0 42
#> 
#> [[4]]
#> [1]  4 42

# However, unlisting the matrix result *does* flatten the matrix
con <- textConnection( content )
fileLineApply( con, function (x) { c(nchar(x), 42) },
                 chunkSize= 2)
#> [1]  8 42 10 42  0 42  4 42
```
