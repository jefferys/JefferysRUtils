#' Replace a null value
#'
#' Returns the value `x`, unless x is `NULL`, in which case it returns either
#' `val` or, if `val` is NULL (the default), it returns the result of calling
#' the function `func`. Additional `...` parameters will be passed to `func`
#' if called. If both a replacement value and function are specified, the
#' replacement value is returned but the function is still called for its side
#' effects. Its return value will be ignored. This can be used e.g.
#' with a "warning" or a logging function to report when a replacement occurs.
#' Leaving both `val` and `func` at their default `NULL` values means this
#' function will have no net effect, i.e. it will replace `NULL` with `NULL`.
#' Note, due to R's argument parsing, if you want to call `func` and pass it
#' unnamed parameters via `...`, `val= NULL` must be set.
#'
#' @param x Check this to see if this is `NULL`.
#' @param val Return this value if `x` is `NULL`. Default is `NULL`.
#' @param func Called to generate and
#'   return a replacement value, unless `val` is set, in which case that is
#'   returned; any value returned by `func` is ignored. Can be a bare function
#'   name, string, or function definition.
#' @param ... Additional parameters to pass to `func`, if any. If any are
#'   unnamed, `val` must be specified or explicitly set `NULL`. If `val` is not
#'   specified, then due to R's parameter parsing, the first unnamed extra
#'   parameter will be used as the value of `val` instead of being passed to the
#'   function. That means it will also be returned as the NULL replacement
#'   value. This "parameter swallowing" is a potential issues with all
#'   functions using "..." with parameters that have default values. Handling of
#'   this may change in future implementations.
#'
#' @return `x`, or if that is `NULL` then `val`, or if that is `NULL` the result
#'   of calling `func` with any parameters `...`, or `NULL` if both `val` and
#'   `func` are `NULL`. If both `val` and `func` are set, `val` will be
#'   returned, but `func` will still be called for any potential side effects.
#'
#' @export
#'
#' @examples
#' fixNull( 42, -1 )
#' #> 42
#'
#' fixNull( NULL, -1 )
#' #> -1
#'
#' fixNull( NULL )
#' #> NULL
#'
#' \dontrun{
#' # Return `val`, calling function only for side effect.
#' # Val is specified, so no issue with unnamed parameters
#' fixNull( NULL, 42, func="warning", "Was NULL" )
#' #> 42
#' #> Warning message:
#' #> In fixNull( NULL, func="warning", "Failed!" ) : Was NULL
#' }
#'
#' # Calling function (with extra parameter) to generate replacement value.
#' # Extra parameter is named, so no issue with unspecified `val`.
#' fixNull( NULL, func= function( num ) { num * log(num) }, num= 42 )
#' #> [1] 156.9821
#'
#' # Calling function (with extra parameters) to generate replacement value.
#' # Some extra  parameters are unnamed, so must specify `val= NULL`.
#' fixNull( NULL, val=NULL, func= paste, "this", "that", sep= "-" )
#' #> [1] "this-that"
fixNull <- function( x, val= NULL, func= NULL, ... ) {
   if ( ! is.null( x )) {
      x
   }
   else {
      retVal <- NULL
      if (! is.null( func )) {
         retVal <- do.call( match.fun( func ), list( ... ))
      }
      if (! is.null( val )) {
         retVal <- val
      }
      retVal
   }
}

#' An ifelse handling NA and NULL too.
#'
#' Tests a Boolean scalar value and returns one of four values:
#' * If `x` is `TRUE`, returns the value of the `true` parameter (TRUE by default).
#' * If `x` is `FALSE`, returns the value of `false` parameter (FALSE by default).
#' * If `x` is `NA`, returns the value of `na` parameter (NA by default).
#' * If `x` is `NULL`, returns the value of `null` parameter (NULL by default).
#'
#' @param x The value to test
#' @param true The value to return if `x` is `TRUE`. Returns `TRUE` by default.
#' @param false The value to return if `x` is `FALSE`. Returns `FALSE` by default.
#' @param na The value to return if `x` is `NA`. Returns `NA` by default.
#' @param null The value to return if `x` is `NULL`. Returns `NULL` by default.
#'
#' @return Depending on the value of x, returns the values of the matching
#' parameter `true`, `false`, `na`, or `null`.
#'
#' @examples
#' ifElseMore( 3 > 2 )
#' #> [1] TRUE
#' ifElseMore( 3 < 2 )
#' #> [1] FALSE
#' ifElseMore( 3 < NA )
#' #> [1] NA
#'
#' ifElseMore( 3 > 2, "it's True", "it's False", "it's NA", "it's Null" )
#' #> [1] "it's True"
#' ifElseMore( 2 > 3, "it's True", "it's False", "it's NA", "it's Null" )
#' #> [1] "it's False
#' ifElseMore( NA > 2, "it's True", "it's False", "it's NA", "it's Null" )
#' #> [1] "it's NA"
#'
#' x <- NULL
#' ifElseMore( x )
#' #> [1] NULL
#' ifElseMore( x, "it's True", "it's False", "it's NA", "it's Null" )
#' #> [1] "it's Null"
#'
#' @export
ifElseMore <- function( x, true= TRUE, false= FALSE, na= NA, null= NULL ) {
   # NULL not handled as a nested ifElse due to problems with NULLs in vectors
   if (is.null(x) && is.null(null)) {
      NULL
   } else if (is.null(x) && ! is.null(null)) {
      null
   } else {
      ifelse( is.na(x),
              na,
              ifelse( x,
                      true,
                      false ))
   }
}