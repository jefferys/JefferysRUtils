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

#' Extract matched substrings
#'
#' You probably want \code{\link{regexprCapture}} as it is likely you are trying
#' to use a regular expression with capture groups. This function parses an
#' already generated match result; it is used by regexprCapture.
#'
#' Extracts the substrings matched by capture groups from a provided
#' match result, i.e from output from \code{\link[base]{regexpr}}, with \code{perl=
#' TRUE}). Will return a matrix of strings with one column for each
#' capture group and one row for each string in the vector matched against. By
#' default will return empty strings if match fails, but can be set to return
#' NAs if desired. Supports named capture groups, matrix columns will be named
#' as appropriate.
#'
#' This is intended for use with \code{\link[base]{regexpr}} to parse a string
#' and extract substrings via capture groups, similar to how
#' \code{\link[base]{regmatches}} is used. If only one string is matched
#' against, then returned matrix will have one row only.
#'
#' Note that regExp with multiple capture groups will need to use greedy and
#' non-greedy matching carefully to avoid the capture groups interfering with
#' each other.
#'
#' @param matchResults The results of a match performed using
#'   \code{\link[base]{regexpr}(regExp, matchText, perl= TRUE)} where
#'   \code{regExp} has capture groups or named capture groups, like
#'   \code{([^:]*)} or \code{(?<beforeColon>[^:]*)}. Will not work with
#'   \code{perl= FALSE}.
#'
#' @param matchText The text originally matched against, a vector of strings.
#'
#' @param use.na By default returns empty strings for all capture groups if the
#'   regExp fails to match. That can not be distinguished from a match with all
#'   capture groups matching nothing, e.g. \code{(?<num>\\d*)}. Setting this
#'   \code{TRUE} causes a failing match to return all NA values instead.
#'
#' @return A matrix with one column for each capture group (with matching
#'   column name for named capture groups) and one row for each string in the
#'   text vector matched to. The value of each cell is the text matched by the
#'   named capture group. If any capture group does not match, all returned
#'   strings are empty for that text vector element (row), or \code{NA} if
#'   \code{use.na= TRUE}
#'
#' @examples
#' regExp <- "(?<key>.+?)\\s*=\\s*(?<value>.+)"
#' data <- c('name = Stuart R. Jefferys', 'email=srj@@unc.edu')
#' matchResults <- regexpr(regExp, data, perl= TRUE)
#' regexprMatches(matchResults, data)
#' #=>      key     value
#' #=> [1,] "name"  "Stuart R. Jefferys"
#' #=> [2,] "email" "srj@@unc.edu"
#'
#' @seealso \code{\link{regexprCapture}} \code{\link{regex}}
#' @export
regexprMatches <- function( matchResults, matchText, use.na=FALSE ) {

   captureNames <- attr(matchResults,'capture.names')
   nrows <- length(matchText)
   ncols <- length(captureNames)
   retMat <- matrix(character(nrows*ncols), nrow = nrows, ncol = ncols, dimnames=list(rep(NULL,nrows),captureNames))
   captureStarts <- attr(matchResults,'capture.start')
   captureLengths <- captureStarts + attr(matchResults,'capture.length') - 1
   for (colPos in 1:ncols) {
      retMat[,colPos] = substr(matchText,captureStarts[,colPos], captureLengths[,colPos])
   }

   # Simple but possibly inefficient to just reset values afterwards.
   if (use.na) {
      for (row in 1:nrows) {
         if (matchResults[row] == -1) {
            retMat[row,] <- rep(NA, ncols)
         }
      }
   }
   return(retMat)
}

#' Extract text with regexp capture groups
#'
#' Applies a (perl) regular expression with capture groups to text strings and
#' returns a matrix. Each matrix column is the text that one capture group
#' matched (in order), each matrix row is the outcome of applying that regexp to
#' one element of the text data. If a capture group does not match, the empty
#' string is returned unless \code{use.na = TRUE} is set, it which case NA is
#' returned. In either case, if a capture group matches nothing (i.e. when * is
#' used to match 0 or more, and 0 match), an empty string is returned.
#'
#' This is implemented using \code{\link{regexprMatches}}
#'
#' @param re The (perl) regular expression as a string, with capture groups. May
#'   use named capture groups (\code{(?<name>...)}). Must double any \code{\\}
#'   used, e.g. zero or more whitespace characters would be \code{(\\s*)}
#'
#' @param data A vector of strings to search in. The rows in the returned matrix
#'   will be the captured text from successive elements of this vector.
#'
#' @param use.na Set TRUE to return NA as the matched text for capture groups
#'   that fail to match
#'
#' @return A matrix with one column per regular expression capture group and one
#'   row per data element. Columns will be named if named capture groups are
#'   used.
#'
#' @examples
#' # Capture group: (...)
#' # Named capture group: (?<name>...)
#' # Lazy quantifier: *?
#' regExp <- "\\s*(?<name>.*?)\\s*<\\s*(?<email>.+)\\s*>\\s*"
#' data <- c('Stuart R. Jefferys <srj@@unc.edu>',
#'           'nonya business <nobody@@nowhere.com>',
#'           'no email', '<just@@an.email>' )
#'
#' regexprCapture(regExp, data)
#' #=> name                  email
#' #=> [1,] "Stuart R. Jefferys" "srj@unc.edu"
#' #=> [2,] "nonya business"     "nobody@nowhere.com"
#' #=> [3,] ""                    ""
#' #=> [4,] ""                    "just@an.email"
#'
#' regexprCapture(regExp, data, use.na=TRUE)
#' #=> name                  email
#' #=> [1,] "Stuart R. Jefferys" "srj@unc.edu"
#' #=> [2,] "nonya business"     "nobody@nowhere.com"
#' #=> [3,] NA                    NA
#' #=> [4,] ""                    "just@an.email"
#'
#' @export
regexprCapture <- function( re, data, use.na = FALSE ) {
   regexprMatches( regexpr(re, data, perl= TRUE), data, use.na= use.na )
}

#' Evaluate and fill string templates
#'
#' Given a vector of strings containing \code{\{\{variables\}\}}, returns a copy
#' replacing the templated fields with the value of the specified variables.
#' Variables must be defined in the calling environment (or the one passed in),
#' or an error will occur. If \code{as.R= TRUE}, then any \code{\{\{R code\}\}} can be used
#' and it will be evaluated to obtain a return value. That is, of course,
#' dangerous if you don't trust the source of your template. All the template
#' code is executed in the same environment, created for this purpose or passed
#' in from the command line. A passed in environment can be used to retrieve
#' variables and functions defined or set in template code.
#'
#' @param x Vector of strings with fields containing variables or code to be
#'   interpolated.
#'
#' @param delim Vector of two string, the first used to signal the start of a
#' template section and the second used to signal the end. These may not be
#' the same, nor have one embedded in the other. By default the open delimiter
#' is \code{\{\{} and the close delimiter is \code{\}\}}.
#'
#' @param as.R Set \code{TRUE} to allow full R code evaluation. By default is
#'   \code{FALSE} and only allows variable substitution. Setting this true is a
#'   security risk when you don't trust the provider of the template text as
#'   much as you trust the person who provided your R code, so it generates a
#'   warning.
#'
#' @param envir The execution environment to be used. Can be used to pass in the
#'   an environment in which variables are defined for use in interpolation. If
#'   not specified, then by default this will be a new environment whose parent
#'   is the caller's environment, as returned by \code{\link{parent.frame}}.
#'   Variables visible in the calling function (or set there) will be available
#'   for use in the template. Note that although R code will normally only set
#'   or change variables in this frame when evaluated, it can set or change
#'   variables at any level, hence malicious or careless \code{as.R= TRUE}
#'   evaluated templates can leak or interfere with other R variables in your
#'   code (or indeed in any other package or even system code). With great power
#'   comes great responsibility.
#'
#' @return A copy of the original vector of strings, but with variable names
#'   replaced with their values, or with the result of evaluating the
#'   interpolated string as R code. Note that everything is returned as a
#'   string, so \code{'{1+1}'} is returned as \code{'2'}.
#'
#' @examples
#' # Template is a single text element (could be multi-line)
#' templateText <- "Dear {{name}}: Please call me at {{phone}}."
#' name <- "John Doe"
#' phone <- "555-555-5555"
#' templateFill( templateText )
#' #=> [1] "Dear John Doe: Please call me at 555-555-5555."
#'
#' # Delimiters can be changed
#' templateText <- "Dear -<[name]>-: Please contact me at -<[email]>-."
#' name <- "John"
#' email <- "the.bobs@@layoffs.com"
#' templateFill( templateText, delim= c( '-<[', ']>-' ))
#' #=> [1] "Dear John: Please contact me at the.bobs@@layoffs.com."
#'
#' # Multiple text elements (each could be multi line)
#' templateText <- c( "ID: {{id}}", "Item: {{name}}", "Description: {{desc}}" )
#' id <- "0001-12"
#' name <- "widget"
#' desc <- "Widget to foo the bar."
#' templateFill( templateText )
#' #=> [1] "ID: 0001-12"
#' #=> [2] "Item: widget"
#' #=> [3] "Description: Widget to foo the bar."
#'
#' # Evaluating R code
#' x <- 21
#' y <- 'Helloooo'
#' templateText <- c(
#'     "Simple: {{1 + 1}}",
#'     "Variables are accessible: {{x *2}}",
#'     "Complex: {{ echo <- function(x) { paste(x,x,sep='...') }; echo(y) }}",
#'     "Code environment is shared: {{ echo( 'Goodbyyyy' ) }}"
#' )
#' templateFill( templateText, as.R= TRUE )
#' #=> [1] "Simple: 2"
#' #=> [2] "Variables are accessible: 42"
#' #=> [3] "Complex: Helloooo...Helloooo"
#' #=> [4] "Code environment is shared: Goodbyyyy...Goodbyyyy"
#' #=> Warning message:
#' #=> In templateFill(templateText, as.R = TRUE) :
#' #=>    Potential security risk: templateFill() is evaluating user-provided
#' #=>    R code If you trust where the template is coming from, you can
#' #=>    suppress this message with suppressWarnings().
#'
#' # Using an environment to provide data and to share results back.
#' env <- new.env()
#' env$x <- 3
#' env[['y']] <- 5
#' templateText <- c(
#'     "x + y = {{x + y}}",
#'     "shared z = x*y = {{(z <- x*y)}}",
#'     "shared function f(x) = x*x = {{f<-function(x) {x*x};f(x)}}"
#' )
#' x<-1; y<-2; z<-3 # Ignored as using env
#' suppressWarnings( templateFill( templateText, as.R= TRUE, envir= env ))
#' #=> [1] "x + y = 8"
#' #=> [2] "shared z = x*y = 15"
#' #=> [3] "shared function f(x) = x*x = 9"
#' env$z
#' #=> [1] 15
#' env$f(3)
#' #=> [1] 9
#' x
#' #=>[1] 1
#'
#' # Template code CAN affect environment
#' x <- "safe command"; y <- "also safe command"
#' templateText<- c(
#'     "x (template) = {{ x <- 'bad command!!!'; x }}",
#'     "y (template) = {{ y <<- 'bad command also!!!'; y }}"
#' )
#' suppressWarnings( templateFill( templateText, as.R= TRUE ))
#' #=> [1] "x (template) = bad command!!!"
#' #=> [2] "y (template) = bad command also!!!"
#' # Template has reached out and mangled a previously safe variable
#' paste( "Running", x, sep= " ")
#' #=> [1] "Running safe command"
#' paste( "Running", y, sep= " ")
#' #=> [1] "Running bad command also!!!"
#'
#' @export
templateFill <- function( x,
                          delim = c( '{{', '}}' ),
                          as.R = FALSE, envir = new.env( parent= parent.frame() )
) {
   if (length(delim) != 2) {
      stop("delim= must have exactly two elements.")
   }
   if (delim[1] == delim[2]) { stop("delim= must have different open and close elements") }
   if (    grepl(delim[1], delim[2], fixed= TRUE)
           || grepl(delim[2], delim[1], fixed= TRUE)
   ) {
      stop("Can't have one of the delimiters embedded in the other.")
   }
   if (as.R) {
      warning( "Potential security risk:",
               " templateFill() is evaluating user-provided R code",
               " If you trust where the template is coming from,",
               " you can suppress this message with suppressWarnings().")
   }

   # Find delimiter positions
   starts <- gregexpr(delim[1], x, fixed= TRUE)
   ends <- gregexpr(delim[2], x, fixed= TRUE)

   # Pre-allocate the returned vector of strings
   retVal <- character(length(x))

   # Process each string in the input vector (possibly 0)
   for (stringNum in 1:length(x)) {
      # Any string without BOTH delimiters is just returned as is
      if (starts[[stringNum]][1] == -1 || ends[[stringNum]][1] == -1) {
         retVal[stringNum] <- x[stringNum]
         next
      }
      # If any string has both delimiters, but has a mismatched number of open
      # and closed delimiters, fail for the whole thing.
      if (length(starts[[stringNum]]) > length(ends[[stringNum]])) {
         stop("Too many ", delim[1], " found in template text element ", stringNum, ".",
              " Probably missing one or more ", delim[2], ".")
      }
      else if (length(starts[[stringNum]]) < length(ends[[stringNum]])) {
         stop("Too many ", delim[2], " found in template text element ", stringNum, ".",
              " Probably missing one or more ", delim[1], ".")
      }
      # Have equal number of paired delimiters, so ready to start. Haven't
      # verified delimiter come in correct order, that will be done as we
      # process them in pairs.

      # Split this string into pieces at each delimiter (begin AND and). Some
      # string pieces may be 0 if the string begins and/or ends with a delimiter
      pieces <- character(2 * length(starts[[stringNum]]) + 1)

      # First piece is string up to first open delimiter
      pieces[1] <- substr(x[stringNum], 1, starts[[stringNum]][1]-1)

      # Remaining pieces come in pairs: between open and close delimiter (the
      # text to process as a template), and, except for the last close delimiter,
      # the part between the close delimiter and the next open delimiter.
      for (fieldNum in 1:length(starts[[stringNum]])) {
         # This pair of delimiters comes in the correct order, or die.
         if (starts[[stringNum]][fieldNum] > ends[[stringNum]][fieldNum]) {
            stop(delim[2], " before ", delim[1], " in string ", stringNum, ".")
         }
         # This is not the last pair of delimiters, so check for next delimiters
         # (the *next* start can't come before this open delimiter's paired close)
         if (length(starts[[stringNum]]) > fieldNum) {
            if (starts[[stringNum]][fieldNum + 1] < ends[[stringNum]][fieldNum]) {
               stop("Nested delimiters not allowed: ", delim[1], " occurs again before ", delim[2], " in string ", stringNum, ".")
            }
         }
         # Yay, we finally have a guaranteed good delimiter pair. Get the contents
         # of the string between these delimiters (the template text) as "field"
         fieldStart <- starts[[stringNum]][fieldNum] + attr(starts[[stringNum]], 'match.length')[fieldNum]
         fieldEnd <- ends[[stringNum]][fieldNum] - 1
         field <- substr(x[stringNum], fieldStart, fieldEnd)

         if (as.R) {
            # Evaluate template text as R code
            pieces[2*fieldNum] <- eval(parse(text=field), envir= envir, enclos=envir)
         }
         else {
            # Evaluate template text as a variable name
            pieces[2*fieldNum] <- get(field, envir= envir, inherits=TRUE)
         }
         nonfieldStart <- ends[[stringNum]][fieldNum] + attr(ends[[stringNum]], 'match.length')[fieldNum]
         if (length(starts[[stringNum]]) > fieldNum) {
            nonfieldEnd <- starts[[stringNum]][fieldNum+1] - 1
         }
         else {
            nonfieldEnd <- nchar(x[stringNum])
         }
         pieces[2*fieldNum + 1] <- substr(x[stringNum], nonfieldStart, nonfieldEnd)
      }
      retVal[stringNum] <- paste0( pieces, collapse="")
   }
   return(retVal)
}



#' Convert strings to vectors of chars.
#'
#' For inputs containing only a single string, returns a character vector of
#' single chars (UTF8s) by default. For inputs containing multiple strings, or if
#' drop= FALSE is set, returns a list of character vectors (of single chars)
#' named for the string. Duplicate names are fine.
#'
#' @param x A vector of strings to convert
#'
#' @param drop Only affects output when \code{x=} is a single element. Set to
#' FALSE to avoid automatic simplification of the one-element list output to a
#' vector.
#'
#' @param use.names By default the list will use the original strings as names.
#' This may not be reasonable for large strings; set this false to just use
#' numeric ordering (the same as the input string order).
#'
#' @return The input strings, split into vectors of single characters. If more
#' than one string is input, or if \code{drop= FALSE} is set this outputs a list
#' of vectors. Vectors will be named for the input string, if possible.
#'
#' @examples
#' toChar( c("ABC", "ABC", "A\u00dfB", 123, "", "x", NA ) )
#' #=> $ABC
#' #=> [1] "A" "B" "C"
#' #=>
#' #=> $ABC
#' #=> [1] "A" "B" "C"
#' #=>
#' #=> $AßB
#' #=> [1] "A" "ß" "B"
#' #=>
#' #=> $`123`
#' #=> [1] "1" "2" "3"
#' #=>
#' #=> [[5]]
#' #=> [1] ""
#' #=>
#' #=> $x
#' #=> [1] "x"
#' #=>
#' #=> $<NA>
#' #=> [1] NA
#'
#' toChar( "ABC" )
#' #=> [1] "A" "B" "C"
#'
#' toChar( c("ABC", "ABC", use.names= FALSE) )
#' #=> [[1]]
#' #=> [1] "A" "B" "C"
#' #=>
#' #=> [[1]]
#' #=> [1] "A" "B" "C"
#'
#' toChar( "ABC", drop= FALSE )
#' #=> $ABC
#' #=> [1] "A" "B" "C"
#'
#' toChar( "ABC", drop= FALSE, use.names= FALSE )
#' #=> [[1]]
#' #=> [1] "A" "B" "C"
#'
#' toChar( 123 )
#' #=> [1] "1" "2" "3"
#'
#' toChar( NULL )
#' #=> character(0)
#'
#' toChar( character(0) )
#' #=> character(0)
#'
#' @export
toChar <- function (x, drop= TRUE, use.names=TRUE) {
   x <- as.character(x)
   if (length(x) == 0) {
      return(character(0))
   }
   if (length(x) == 1) {
      charString <- intToUtf8( utf8ToInt(x), multiple= TRUE )
      if (length(charString) == 0) {
         charString <- ""
      }
      if (drop) {
         return(charString)
      }
      else {
         charString <- list(c(charString))
         if (use.names) {
            names(charString) <- x
         }
         return(charString)
      }
   }
   else {
      charList <- sapply(
         x,
         function (str) {
            intToUtf8( utf8ToInt(str), multiple= TRUE )
         },
         USE.NAMES= use.names
      )
      charList[sapply(charList, function(vec) {length(vec) == 0} )] <- ""
      return(charList)
   }
}

#' Reverse a string
#'
#' @param x A vector of strings to reverse. Returns NA for NA and empty for empty.
#' If x=NULL or character(0), returns character(0).
#'
#' @return A vector of strings, each reversed. This is utf8 aware.
#'
#' @examples
#' revString( "ABC" )
#' #=> revString( "ABC" )
#'
#' revString( c( "ABC", "ABC", "A\u00dfB", 123, "", "x", NA ))
#' #=> [1] "CBA" "CBA" "BßA" "321" ""    "x"   NA
#'
#' revString( NULL )
#' #=> character(0)
#'
#' revString( character(0) )
#' #=> character(0)
#'
#' @export
revString <- function (x) {
   x <- as.character(x)
   if (length(x) == 0) {
      return(character(0))
   }
   if (length(x) == 1) {
      charString <- intToUtf8( rev(utf8ToInt( x )))
      return(charString)
   }
   else {
      return( sapply( x, function(str) { intToUtf8( rev(utf8ToInt( str ))) }, USE.NAMES= FALSE ))
   }
}


#' Longest common prefix
#'
#' Finds and returns the longest prefix common to all strings in a character
#' vector. Can be set to ignore case, in which case the returned common string
#' will be in lower case. If any strings are \code{NA}, this returns \code{NA}
#' unless \code{dropNA= TRUE} is set, which will drop \code{NA}s before
#' checking for a common prefix.
#'
#' @param x The strings to find the longest common prefix for.
#'
#' @param ignoreCase Set this true to match prefixes even if they differ in case.
#'
#' @param dropNA Set this true to ignore \code{NA}s when searching for a common
#' prefix.
#'
#' @return The common prefix, if any, or "", if no common prefix can be found.
#'
#' @examples
#' commonPrefix( c( "ABCDE", "ABC", "ABc" ))
#' #=> [1] "AB"
#'
#' commonPrefix( c( "ABC", "abc", "def" ))
#' #=> [1] ""
#'
#' commonPrefix( c( "ABCDE", "ABC", "" ))
#' #=> [1] ""
#'
#' commonPrefix( c( "ABCDE", "ABC", NA ))
#' #=> [1] NA
#'
#' commonPrefix( c( "A\u00dfCDE", "A\u00dfC", "A\u00dfc" ))
#' #=> [1] "Aß"
#'
#' commonPrefix( c("ABCDE", "ABC", "ABc" ), ignoreCase= TRUE )
#' #=> [1] "abc"
#'
#' @seealso \code{\link{commonSuffix}}
#' @export
commonPrefix <- function (x, ignoreCase= FALSE, dropNA= FALSE) {
   # Get first and last lexically ordered string in x. Faster than sorting.
   # Have to be careful about shorter strings, e.g. ABC ab abc

   if (dropNA) {
      x <- x[! is.na(x)]
   }

   minCharCount <- min(nchar(x))

   if (ignoreCase) {
      candidateSubstrings <- tolower( substr(x, 1, minCharCount ))
   }
   else {
      candidateSubstrings <-          substr( x, 1, minCharCount )
   }
   lowSubStr  <- min( candidateSubstrings )
   highSubStr <- max( candidateSubstrings )

   # Convert to character vectors
   lowCharInts  <- utf8ToInt( lowSubStr )
   highCharInts <- utf8ToInt( highSubStr )

   # Find position of first non matching char, return substring up to but
   # not including that char.
   firstNonMatch <- match(c(FALSE, NA), lowCharInts == highCharInts, nomatch= minCharCount + 1)
   commonSubstring <- substr(x[1], 1,  firstNonMatch - 1)

   if (ignoreCase) {
      tolower( commonSubstring )
   }
   else {
      commonSubstring
   }
}

#' Longest common suffix
#'
#' Finds and returns the longest suffix common to all strings in a character
#' vector. Can be set to ignore case, in which case the returned common string
#' will be in lower case. If any strings are \code{NA}, this returns \code{NA}
#' unless \code{dropNA= TRUE} is set, which will drop \code{NA}s before
#' checking for a common suffix
#'
#' @param x The strings to find the longest common suffix for.
#'
#' @param ignoreCase Set this true to match suffixes even if they differ in case.
#'
#' @param dropNA Set this true to ignore \code{NA}s when searching for a common
#' suffix
#'
#' @return The common suffix, if any, or "", if no common suffix can be found.
#'
#' @examples
#' commonSuffix( c( "ABCDE", "CDE", "cDE" ))
#' #=> [1] "DE"
#'
#' commonSuffix( c( "ABC", "abc", "def" ))
#' #=> [1] ""
#'
#' commonSuffix( c( "ABCDE", "CDE", "" ))
#' #=> [1] ""
#'
#' commonSuffix( c( "ABCDE", "ABCDE", NA ))
#' #=> [1] NA
#'
#' commonSuffix( c( "A\u00dfC", "A\u00dfC", "a\u00dfC" ))
#' #=> [1] "ßC"
#'
#' commonSuffix( c("ABCDE", "CDE", "cDE" ), ignoreCase= TRUE )
#' #=> [1] "cde"
#'
#' @seealso \code{\link{commonPrefix}}
#' @export
commonSuffix <- function (x, ignoreCase= FALSE, dropNA= FALSE) {
   # Have to reverse strings to use same algorithm as for prefix. Doing so probably
   # makes this significantly slower. Maybe another algorithm would be better?
   revStrings <- revString( x )

   # As reversed, common prefix is actually the common suffix, reversed
   revSuffix <- commonPrefix(revStrings, ignoreCase= ignoreCase, dropNA= dropNA)
   intToUtf8( rev( utf8ToInt( revSuffix )))
}