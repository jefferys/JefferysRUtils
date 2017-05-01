# Functions for working with file contents, i.e. reading and writing.
# Paths and file metadata functions should be elsewhere.


#' Create a temporary text file
#'
#' Creates a temporary text file whose name will be unique and whose context is
#' whatever lines of text are specified as a character vector. By default these
#' are created in R's session temporary directory. This directory and all its
#' contents are removed automatically when the R session ends. File names
#' contain random characters to make them unique, a prefix and a suffix can be
#' specified. An (existing) directory can also be specified, but unless the
#' directory is a subdirectory of `tempdir()`, it will *not* be automatically
#' removed.
#'
#' @param lines The lines of text to put into the file.
#' @param namePrefix The constant part of the temporary file's name, defaults to
#'   "temp". Normally followed by some random hex digits to ensure name is
#'   unique.
#' @param extension The file extension part of the temporary file's name,
#'   defaults to ".txt". The "." must be explicitly specified; this is really
#'   just a suffix.
#' @param tempDir The directory the file will be created in. Defaults to R's
#'   per-session temporary directory. If this is specified it must exist. If it
#'   is not a sub-directory of the `tempdir()`, it will *not* be removed
#'   automatically.
#' @param eol The end of line character to use when writing content to the file.
#'   Defaults to the R platform default. Note, this is not a separator; the last
#'   line written to the file will be terminated with this character.
#'
#' @return Returns the full-path filename to the temporary text file, which has
#'   been created with the provided content. This filename may not be in
#'   canonical form. By default this will look like:
#'   \<tempDir\>/temp\<random\>.txt. Can change the default namePrefix= "temp"
#'   and the extension= ".txt" settings.
#'
#' @examples
#' tempFile <- makeTempFile( c("one fish", "two fish") )
#' readLines( tempFile )
#' #> [1] "one fish" "two fish"
#'
#' # Can create files that use other platform line endings.
#' # (readLines() converts automatically to two lines)
#' file <- makeTempFile( c("one fish", "two fish"), eol="\r" )
#' readLines( file )
#' #> [1] "one fish" "two fish"
#'
#' # Can create files that use weird line endings
#' file <- makeTempFile( c("one fish", "two fish"), eol=" <>< " )
#' # Note: " <>< " is not a standard eol, so readLines() returns everything as one
#' #   string, plus would warn about not ending file with a normal eol character
#' #   if warn= FALSE was not set.
#' readLines( file, warn= FALSE )
#' #> [1] "one fish <>< two fish <>< "
#'
#' @export
makeTempFile <- function( lines= character(0),
                          namePrefix= "temp", extension= ".txt",
                          tempDir= tempdir(), eol= "\n" ) {
   if (! dir.exists( tempDir )) {
      stop( paste0( "tempDir= directory does not exist: '", tempDir, "'" ), call. = FALSE )
   }

   fileName <- tempfile( pattern = namePrefix, tmpdir = tempDir, fileext= extension )
   writeLines( text= lines, con= fileName, sep= eol )
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

