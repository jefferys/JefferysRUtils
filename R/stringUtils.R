#' Binary operator versions of paste and paste0
#'
#' Like most operators, can split across lines with operator at front of second
#' line only if inside parenthesis. Unlike most operators, this is pretty
#' likely to happen. See Examples.
#'
#' @param x The first object to paste
#' @param y The second object to paste
#'
#' @return Returns the result of pasting the two objects together:
#' \preformatted{
#'     x \%p\% y == paste0(x, y)
#'     x \%pp\% y == paste(x, y)
#' }
#'
#' @examples
#' "Hello, " %p% "world!" == "Hello, world!"
#' "Hello," %pp% "world!" == "Hello, world!"
#' name <- "Amy"
#' "Hello," %pp% name %p% "!" == "Hello, Amy!"
#' "Hello," %pp%
#'    "world!" == "Hello, world!"
#' ("Hello,"
#'     %pp% "world!") == "Hello, world!"
#' # "Hello,"
#' #    %pp% "world!" == "Hello, world!"
#' # Error: unexpected SPECIAL in "     %pp%"
#'
#' @export
`%p%` <- function(x, y) { paste0(x, y) }

#' @rdname grapes-p-grapes
#' @export
`%pp%` <- function(x, y) { paste(x, y) }
