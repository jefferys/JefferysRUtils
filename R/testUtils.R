# Testing utilities for use with testthat

#' Extended equality tests
#'
#' General test fixture - extend expect_equal to support \code{NULL}.
#'
#' @param got The object to test
#' @param want The expected value, may be \code{NULL}
#' @param label Object label
#' @param info Extra info
#' @param ... Extra parameters passed to \code{\link{all.equal}}
#' @return Called only for its side effects during testing.
#' @examples
#' \dontrun{
#' ### Pass
#' expect_equalOrNull( 1, 1 )
#' expect_equalOrNull( list(a=1,b=2), list(b=2, a=1))
#' x <- list(a=1, b=2)["c"]
#' expect_equalOrNull( x, NULL )
#'
#' ### Fail
#' expect_equalOrNull( 1, "1" )
#' expect_equalOrNull( 1, NULL )
#' expect_equalOrNull( NULL, 1 )
#' }
#' @export

expect_equalOrNull <- function(got, want, label= NULL, info= NULL, ...) {
	if (is.null(want)) {
		testthat::expect_null(got, label=label, info=info)
	}
	else {
		testthat::expect_equal(got, want, label=label, info=info, ...)
	}
}