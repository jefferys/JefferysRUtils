## Functions for working with tables

#' Is a value duplicated?
#'
#' Checks each value in a vector, data frame, matrix, array, etc,  to see if it
#' repeats. If it does, it and all its repeated values are reported as
#' duplicates. `NA` and `NaN` are treated as values that can be duplicated or
#' not; to consider them (and potentially `""`, and/or `NaN`, etc) as not
#' duplicates, use `incomparables=`.
#'
#' Basically a simple wrapper around the algorithm:
#' `is.duplicated(x) == duplicated(x) | duplicated(x, fromLast = TRUE)`
#' and replicates the potentially inconsistent behavior of duplicated with
#' respect to matrices and arrays, especially degenerate matrices and arrays.
#'
#' @param x The object to evaluate for duplicates. Handled generically so can
#' be vector-like (values), data.frame-like (rows), or matrix-like (rows, or
#' other dimension if specified via `MARGIN=<INT>` ).
#' @param ... Extra parameters like `MARGIN` for the appropriate `duplicated.<x>()`
#' implementation, except for `fromLast`, which is used internally and will
#' result in an error.
#'
#' @return Logical vector indicating for each element of x if it is a repeated
#' value. For data frames, one element per row. For matrix and arrays, what
#' is returned depends on MARGIN and how degenerate your matrix or array is.
#'
#'
#' @examples
#' isDuplicated( c( 1, 2, 3, 4, 3, 1 ))
#' #> [1]  TRUE FALSE  TRUE FALSE  TRUE  TRUE
#'
#' mat <- matrix( c( 1, 3,  1,
#'                  NA, 3, NA,
#'                   2, 3,  2 ), nrow= 3, byrow= TRUE )
#' isDuplicated( mat, MARGIN= 2 )
#' #> [1]  TRUE FALSE  TRUE
#'
#' @seealso `duplicated()`
#'
#' @export
isDuplicated <- function( x, ... ) {
   duplicated(x, fromLast= FALSE, ...) | duplicated(x, fromLast = TRUE, ...)
}
