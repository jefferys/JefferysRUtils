# Functions for working with file contents, i.e. reading and writing.
# Paths and file metadata functions should be elsewhere.

makeTempFile <- function( lines= character(0) ) {
   fileName <- tempfile()
   writeLines(lines, fileName)
   return( fileName )
}

#' Read a file's lines in as vector
#'
#' Essentially a convenience wrapper around \code{\link{readLines}}. Reads a
#' file's lines into a string vector. Can skip comments if specify a comment
#' string, and drops the header line if told to. Inspired by perl's "slurp"
#' mode.
#'
#' @param file The file to read
#'
#' @param commentString If specified, any line beginning with this comment
#'   string will be dropped (leading whitespace is ignored). This is parsed as a
#'   regualr expression, but that shouldn't be a problem for most normal line
#'   comment strings (\code{"#"}, \code{"##"}, \code{"//"}, \code{";"}), leading
#'   whitespace is ignored. If null or empty, will be ignored. Backslashes
#'   probably need to be doubled.
#'
#' @param skipLines If specified, skip this many lines at the start of the file.
#'   This will be done after comments are filtered, so don't skip lines if
#'   they are also comments.
#'
#' @return The file's content as a string vector, one element per line, including
#'   empty lines as empty strings, but excluding filtered lines. If no lines are
#'   left after dropping comments and skipping the headers, will return
#'   \code{character(0)}
#'
#' @section Warnings:
#'
#' \describe{
#'    \item{
#'       \command{Skipping more lines, \var{skip}, than lines to skip, \var{lines}.}
#'    }{
#'       There are fewer lines left (after skipping comments) than you asked to
#'       skip. Will return \code{character(0)} to indicate no kept lines/strings.
#'    }
#' }
#'
#' @export
slurp <- function( file, commentString= NULL, skipLines= 0 ) {

   if (! is.null(commentString) && (commentString == "" || is.na(commentString))) {
      commentString <- NULL
   }

   dat <- readLines( file )
   if (length(dat) < 1) {
      return(dat)
   }

   if (! is.null(commentString)) {
      dropRE <- paste0("^\\s*", commentString)
      dat <- dat[! grepl(dropRE, dat, perl= TRUE) ]
   }
   if (length(dat) < 1) {
      return(dat)
   }

   if (skipLines > length(dat)) {
      message = paste0( "Skipping more lines, ", skipLines,
                        ", than lines to skip, ", length(dat), ".")
      warning(message)
   }
   if (skipLines >= length(dat)) {
      return(character(0))
   } else if ( skipLines == 0 ) {
      return(dat)
   } else {
      dat <- dat[ (skipLines + 1):length(dat) ]
      return(dat)
   }
}

