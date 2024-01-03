# statUtils.R - Simple stat functions


#' Cumulative probability distribution segments
#'
#' Calculates the difference between two values of a cumulative probability
#' distribution. Will result in negative probabilities when cdf(b) < cdf(a),
#' e.g. when the ends of the cdf probability segment are reversed.
#'
#' @param a Vector of the left sides of segments. Must be the same length as
#'   b=, or a single value. If a single value, that value will be used as the
#'   left side of all segments
#' @param b Vector of the right sides of segments. Must be the same length as
#'   a=, or a single value. If a single value, that value will be used as the
#'   right side of all segments.
#' @param dist Name of distribution to use, one of: "normal". Will be partially
#'   matched, case insensitively, but must match uniquely.
#' @param ... Distribution parameter, e.g. mean= and sd= for the normal
#'   distribution.
#'
#' @return The probability of each of the specified line segments. Note that
#'   for segments specified backwards like `b[i] < a[i]`, the corresponding
#'   probabilities will be negative.
#'
#' @examples
#' # P( x > 0 ), standard normal distribution
#' cdfDiff( 0, Inf, "norm" )
#' #> [1] 0.5
#'
#' # P( x > 0 ), standard normal distribution (backwards)
#' cdfDiff( Inf, 0, "norm" )
#' #> [1] -0.5
#'
#' # P( x < 2.1 ), normal with mean= 2.1 ( and sd= 1)
#' cdfDiff( -Inf, 2.1, "norm", mean= 2.1 )
#' #> [1] 0.5
#'
#' # P( x = -Inf, x < 0, x < Inf ), standard normal distribution
#' cdfDiff( -Inf, c(-Inf, 0, Inf), "norm" )
#' #> [1] 0.0 0.5 1.0
#'
#' # Mix of forwards and backwards
#' cdfDiff( c(-Inf, -2.1, Inf), c(-2.1, -Inf, -2.1), "norm", mean=-2.1 )
#' #> [1]  0.5 -0.5 -0.5
#'
#' # P( x < 2 or x > 2 ) standard normal
#' sum(cdfDiff( c(-Inf, 2), c(-2, Inf), "norm"))
#' #> 0.04550026
#' @export
cdfDiff <- function (a, b, dist, ... ) {

   ## Validate parameters

   if (missing(a) || missing(b) || missing(dist)) {
      stop( paste0( "cdfDiff() requires at least 3 parameters; ",
                    "'a', 'b', and 'dist'." ))
   }
   # Ensure a and b same length vectors
   if (length(a) == 0) {
      stop( "cdfDiff() parameter 'a' may not be empty." )
   }
   if (length(b) == 0) {
      stop( "cdfDiff() parameter 'b' may not be empty." )
   }
   if ( length(a) != length(b) ) {
      if (length(a) == 1) {
         a <- rep.int(a, length(b))
      } else if (length(b) == 1) {
         b <- rep.int(b, length(b))
      } else {
         stop( paste0( "cdfDiff() parameters 'a' and 'b' must be the ",
                       "same length when neither is length 1." ))
      }
   }

   # Get the distribution name to use with p<dist>
   # allowing abbreviations or full-length names
   knownDist <- list(
      normal= "pnorm"
   )
   distName <- pmatch( tolower(dist), names(knownDist) )
   if (is.na(distName)) {
      stop( paste0( "Specified 'dist' parameter for cdfDiff() is ",
                    "unknown or unsupported: \"", dist, "\"." ))
   }

   distFunc <- knownDist[[distName]]
   extParam <- list(...)

   # caclulate the cumulative probabilites of the provided end-points
   pa <- sapply( a, function( X, dist= distFunc, ext= extParam ) {
      do.call( dist, c( X, extParam ))
   })
   pb <- sapply( b, function( X, dist= distFunc, ext= extParam ) {
      do.call( dist, c( X, extParam ))
   })

   # Return the difference
   return( pb - pa )
}