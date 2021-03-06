% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stringUtils.R
\name{commonPrefix}
\alias{commonPrefix}
\title{Longest common prefix}
\usage{
commonPrefix(x, ignoreCase = FALSE, dropNA = FALSE)
}
\arguments{
\item{x}{The strings to find the longest common prefix for.}

\item{ignoreCase}{Set this true to match prefixes even if they differ in case.}

\item{dropNA}{Set this true to ignore \code{NA}s when searching for a common
prefix.}
}
\value{
The common prefix, if any, or "", if no common prefix can be found.
}
\description{
Finds and returns the longest prefix common to all strings in a character
vector. Can be set to ignore case, in which case the returned common string
will be in lower case. If any strings are \code{NA}, this returns \code{NA}
unless \code{dropNA= TRUE} is set, which will drop \code{NA}s before
checking for a common prefix.
}
\examples{
commonPrefix( c( "ABCDE", "ABC", "ABc" ))
#=> [1] "AB"

commonPrefix( c( "ABC", "abc", "def" ))
#=> [1] ""

commonPrefix( c( "ABCDE", "ABC", "" ))
#=> [1] ""

commonPrefix( c( "ABCDE", "ABC", NA ))
#=> [1] NA

commonPrefix( c( "A\u00dfCDE", "A\u00dfC", "A\u00dfc" ))
#=> [1] "Aß"

commonPrefix( c("ABCDE", "ABC", "ABc" ), ignoreCase= TRUE )
#=> [1] "abc"

}
\seealso{
\code{\link{commonSuffix}}
}
