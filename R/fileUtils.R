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
#'   regular expression, but that shouldn't be a problem for most normal line
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


#' Merge text files adding names
#'
#' Sequentially concatenates text files adding the source filename to the start
#' of each line. Allows files to have headers if the number of header lines is
#' known and the same in all files. Headers will be taken from the first file,
#' others are ignored other than triggering a warning for each that does not
#' match the first file. The output file path can be specified, or it will be
#' created as a temporary file. Instead of prefixing source filenames, a vector
#' of strings can be specified.
#'
#' It is possible to just concatenate files without a prefix if all applicable
#' values are set to the empty string (i.e. \code{names}, \code{delim},
#' and possibly \code{colName}.) A blank line for empty files will be included
#' only if \code{keepEmpty} is set \code{TRUE}.
#'
#' @param inFiles *REQ* The file paths to concatenate.
#' @param outFile The file paths to use for output. Defaults to a temporary file
#'   named `<tempdir()>/merged<random>.tmp`. If the file already exists, it will be
#'   overwritten.
#' @param delim The separator between the prefixed file name column and the
#'   source file lines. Defaults to a tab, \\t.
#' @param headerLines The number of header lines. Defaults to 0. All files must
#'   have the same number of header lines. It is an error if a file has fewer lines
#'   than required by this parameter. A warning is generated for each file whose
#'   header differ from the first.
#' @param colName The header for the name column, if \code{headerLines} > 0. Every
#'   *header* line will have this prefixed, separated by \code{delim}.
#'   Default is FILE.
#' @param keepEmpty Set \code{TRUE} to have empty files treated as if they contained a
#'   single empty line. Results in a line in the output file with just the name
#'   and \code{delim} By default empty files are ignored. For files with headers,
#'   empty files are those that contain no lines other than the header (which
#'   should end with an EOL character.).
#' @param names The names to prefix to the output lines. By default this will be
#'   the \code{inFiles}. Must be a vector of the same length as \code{inFiles}.
#'
#' @return Returns the output file name, important if the output is created as an
#'   temp file.
#'
#' @section Errors:
#'
#' \describe{
#'    \item{
#'       \command{Must specify at least one input file.}
#'    }{
#'       If no files are specified, function will exit with error.
#'    }
#'    \item{
#'       \command{Not enough lines in file to have expected header: "\var{file}".}
#'    }{
#'       There are fewer lines in the file  "\var{file}" than header lines, so
#'       it can't possibly have the same header, let alone any data.
#'    }
#'    \item{
#'       \command{Parameters inFiles= and names= must be vectors of the same length.}
#'    }{
#'       Since names are being used instead of file paths in the output file, it
#'       does not make sense to allow wrapping here. If you want the same name for
#'       multiple files or the same file with multiple names, just include it in
#'       the relevant parameter more than once.
#'    }
#' }
#'
#' @section Warnings:
#'
#' \describe{
#'    \item{
#'       \command{File headings differ between first file and "\var{file}"}
#'    }{
#'       If \code{headingLines} is set (> 0), that many lines will be read from
#'       the first file and used as the heading in the output file. Each following
#'       file is then checked to ensure it has the same heading, If the heading does
#'       not match, this warning is signaled.
#'    }
#' }
#'
#' @examples
#' # Create a couple of temp files to merge
#' header <- "DESC | THING"
#' contentA <- c("One | fish,", "two | fish;")
#' contentB <- c("red | fish,", "blue | fish.")
#' inFileA <- makeTempFile( c( header, contentA ))
#' inFileB <- makeTempFile( c( header, contentB ))
#' empty <- makeTempFile( header )
#'
#' # Merge files
#' # tempFile <- mergeFiles( c(inFileA, empty, inFileB),
#' #                        names= c("A", "B"), headerLines= 1L )
#' # Error as not matching files to names
#' tempFile <- mergeFiles( c(inFileA, empty, inFileB),
#'                         names= c("A", "B", "C"), headerLines= 1L )
#' readLines(tempFile)
#' #> [1] "FILE\tDESC | THING"
#' #> [2] "A\tOne | fish,"
#' #> [3] "A\ttwo | fish;"
#' #> [4] "C\tred | fish,"
#' #> [5] "C\tblue | fish."
#'
#' tempFile <- mergeFiles(
#'    c(inFileA, empty, inFileB), names= c("A", "B", "C"), headerLines= 1L,
#'    colName= 'stanza', delim= ": ", keepEmpty= TRUE )
#' readLines(tempFile)
#' #> [1] "stanza: DESC | THING"
#' #> [2] "A: One | fish,"
#' #> [3] "A: two | fish;"
#' #> [4] "B: "
#' #> [5] "C: red | fish,"
#' #> [6] "C: blue | fish."
#'
#' @export
mergeFiles <- function(
   inFiles,
   outFile= tempfile(pattern= 'merged', fileext = ".tmp" ),
   names= inFiles,
   delim= "\t",
   headerLines= 0L,
   colName= "FILE",
   keepEmpty= FALSE
) {
   if ( length(inFiles) < 1 ) {
      stop( "Must specify at least one input file.")
   }
   if ( length(inFiles) != length(names) ) {
      stop( "Parameters inFiles= and names= must be vectors of the same length.")
   }
   # Stand-alone connections remember file positions
   outCon <- file( outFile, open= "wt" )

   header <- NULL

   for( fileNum in 1:length(inFiles) ) {

      # Read and close as will handle data separately.
      # Also grabbing first data line, if any, to handle empty files up front.
      firstLines <- readLines( inFiles[fileNum], n= (headerLines + 1L) )
      if ( length(firstLines) < headerLines ) {
         close( outCon )
         stop( paste0( 'Not enough lines in file to have expected header: "',
                       names[fileNum], '".' ), call.= FALSE)
      }

      # Headers
      if (headerLines > 0) {

         # On first file, want to write headers, on future passes, just check them.
         if (is.null(header)) {
            header <- firstLines[1:headerLines]
            writeLines( paste( colName, header, sep= delim ),
                        con= outCon )
         }
         else {
            repeatHeader <- firstLines[1:headerLines]
            if (! identical(header, repeatHeader)) {
               warning( paste0( 'File headings differ between first file and "',
                                inFiles[fileNum], '".' ), call.= FALSE )
            }
         }
      }

      # If no data, unless keepEmpty was set to TRUE, done, whether have headers or not.
      if ( length(firstLines) == headerLines && ! keepEmpty) {
         next
      }

      # If not done with file, want to write the rest of the data, even if was
      # empty. Empty files or files with just one line will be reported the same,
      # which why we checked explicitly earlier. Read twice to advance file
      # position pointer and skip header, if any.
      inCon <- file( inFiles[fileNum], open= 'rt' )
      readLines( inCon, n= headerLines )
      writeLines( paste( names[fileNum], readLines(con= inCon), sep= delim), con= outCon )
      close(inCon)
   }

   close(outCon)
   return(outFile)
}

#' Test for absolute path
#'
#' Checks a vector of paths to see if they are absolute paths. Returns a
#' corresponding logical vector that is \code{TRUE} for absolute paths and
#' \code{FALSE} for others. Implementation uses a regular expression so this
#' may not always work correctly, especially in non-ascii settings. No
#' checks are made to see if a path exists nor if it is even a valid path.
#'
#' @param paths A character vector to check to see if which are potentially
#'   absolute paths.
#'
#' @return A logical vector that is \code{TRUE} for absolute \code{paths= },
#'   \code{FALSE+} for most other things (including \code{NA} and empty string),
#'   and an empty vector if \code{paths= } is empty or \code{NULL}.
#'
#' @examples
#' # All absolute paths
#' is.absolutePath( c( "~",  "/", "\\", "~/bob",  "/bob", "\\bob" ))
#' is.absolutePath( c("C:\\", "c:/", "C:\\bob", "c:/bob" ))
#' is.absolutePath( c(".", "./", ".\\", "./bob", ".\\bob" ))
#' is.absolutePath( c(".", "./", ".\\", "./bob", ".\\bob" ))
#' is.absolutePath( c("..", "../", "..\\", "../bob", "..\\bob" ))
#' is.absolutePath( c("/[](){}''|;:::Invalid but absolute!" ))
#'
#' # None are absolute paths
#' is.absolutePath( c("", "bob/../bob", "bob\\..\\bob" ))
#' is.absolutePath( c("bob", "bob\\bob", "bob/bob", ".bob" ))
#' is.absolutePath( c( NA, "NA", "BETA_ß_" ))
#' is.absolutePath( list( x= "hello" ))
#'
#' # empty logical returned
#' is.absolutePath( NULL )
#' is.absolutePath( character( 0 ))
#'
#' @export
is.absolutePath <- function( paths ) {
   grepl( "^[~/\\]|^[A-Za-z]:[\\/]|^\\.(\\.)?$|^\\.(\\.)?[/\\]", paths )
}

#' Read table-like files into a data frame.
#'
#' This is a convenience wrapper around \code{\link{read.table}} for reading
#' tab delimited files. Parsing is the same as with
#' \code{\link{read.table}}, except the separator is \code{"\t"}, headers are by
#' default assumed to be present, and strings are automatically read in as
#' strings. You can change these defaults or any other \code{\link{read.table}}
#' parameter as they are passed through via \code{...}, but you should probably
#' use the actual \code{\link{read.table}} function if you change \code{sep}.
#'
#' @param file The path to the tab-delimited file.
#' @param header Default is \code{TRUE} as most table-like files have headers.
#' @param sep The separator defaults to \code{"\t"} for these tab-delimited files.
#' @param stringsAsFactors Set \code{TRUE} to convert string columns to factors.
#' @param ... Passes other parameters through to \code{\link{read.table}}
#'
#' @return A data frame corresponding to the *.tsv file as specified by the
#' parameters above and the defaults from \code{\link{read.table}}. See
#' \code{\link{read.table}} for details.
#'
#' It is a fatal error if the file does not exist, can not be read, or is empty.
#'
#' @seealso read.table
#' @examples
#' # Create a temp.tsv file to read
#' tsvFile <- makeTempFile( ext=".tsv", lines= c(
#'     "name\tval\tok",
#'     "A\t1\tTRUE",
#'     "B\t2\tFALSE"
#' ))
#' read.tsv( tsvFile )
#' #>   name val    ok
#' #> 1    A   1  TRUE
#' #> 2    B   2 FALSE
#'
#' # A temp.tsv file without a header
#' tsvFile <- makeTempFile( ext=".tsv", lines= c(
#'     "A\t1\tTRUE",
#'     "B\t2\tFALSE"
#' ))
#' read.tsv( tsvFile )
#' #>   V1 V2    V3
#' #> 1  A  1  TRUE
#' #> 2  B  2 FALSE
#'
#' # Pass-through setting column names.
#' tsvFile <- makeTempFile( ext=".tsv", lines= c(
#'     "A\t1\tTRUE",
#'     "B\t2\tFALSE"
#' ))
#' read.tsv( tsvFile, header=FALSE, col.names= c( "name", "val", "ok" ))
#' #>   name val    ok
#' #> 1    A   1  TRUE
#' #> 2    B   2 FALSE
#'
#' @export
read.tsv <- function ( file, header=TRUE, sep= "\t",
                       stringsAsFactors= FALSE, ... ) {
   if (! file.exists(file)) {
      stop( "No such file '" %p% file %p% "'." )
   }
   df <- utils::read.table( file=file, header=header, sep= sep,
                     stringsAsFactors= stringsAsFactors, ... )
   rownames(df) <- NULL
   df
}

#' Test paths to see if they are for a directory
#'
#' If a path ends in "/" or exists and is a directory, it is a directory path. To
#' do a pure lexical check, which just looks for the ending "/" and ignores the
#' file system, set `lexical= TRUE`. Note that paths ending in "/" are directory
#' paths even when they don't exist and even if they could not be created due
#' to illegal characters or the existence of a file with that name (sans the
#' terminal "/"). e.g. `some/path` as a file does not affect `some/path/` being
#' reported as a directory.
#'
#' Any leading "~" is expanded a la `path.expand()`, but `Sys.glob()` is not used,
#' e.g. "*" is just a regular character.
#'
#' @param paths Vector of paths to check and see if they are directories.
#' @param lexical Set TRUE to skip secondary check of the file system for
#'    paths that don't end in "/" but might already exist as directories.
#'
#' @return Returns a logical vector, TRUE for paths to treat as directories,
#'    FALSE otherwise.
#'
#' @seealso
#' - `dir.exists()`: to just check if a directory exists.
#' - `dirname()`: to extract the directory part of a file path (ignores end "/").
#' - `Sys.glob()`: to expand glob patterns into a vector of paths (including
#'     optionally adding a terminal "/" to directories).
#' - `path.expand()`: to expand leading "~" to home directory, where supported.
#'
#' @examples
#'
#' # By default, only false if not ending in "/" and not an existing directory
#' isDirPath( c("./", ".", "noSuchDir/", "noSuchDir", "~", "*" ))
#' #> [1] TRUE, TRUE, TRUE, FALSE, TRUE, FALSE
#'
#' # With lexical=TRUE, only cares about ending "/"
#' isDirPath( c( "./", ".", "noSuchDir/", "noSuchDir", "~", "*" ),
#'            lexical= TRUE )
#' #> [1] TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE
#'
#' @export
isDirPath <- function( paths, lexical= FALSE ) {
   retVal <- substr( paths, nchar( paths ), nchar( paths )) == "/"
   if (! lexical) {
      retVal[ ! retVal ] <- dir.exists( paths[ ! retVal ])
   }
   retVal
}

#' Validate a file against a checksum file
#'
#' Validates a file using a specified checksum algorithm against a file that
#' contains just the expected checksum result. By default looks for a file
#' in parallel to the file tested, except with an additional extension matching
#' the checksum algorithm. Can specify what to do after testing match through
#' use of the `as=` argument. Supports returning TRUE/FALSE, a string message,
#' or signaling a message via `stop()`, `warning()`, or `message()`. To support
#' testing without throwing errors, any errors such as missing file are reported
#' the same as an error triggered by a mismatched checksum, except it causes NA
#' to be returned with `as=test`.
#'
#' @param file The file to validate
#' @param algo The checksum algorithm to use (see `digest::digest()`).
#' @param checkFile The name of the file holding the expected checksum value.
#' If `NULL`, will attempt to guess; assumes named the same as `file=`, except
#' with an additional extension matching `algo=`. If not found, looks for a
#' hidden checkFile (with a leading "."). e.g. an md5 checkFile for
#' `file.txt` would be guessed as `file.txt.md5` or `.file.txt.md5`.
#' @param as How the results of the checksum test are reported. `as="test"`
#'   returns `TRUE` if the checksums match, `FALSE` if they do not, and `NA` if
#'   there is an error looking for the requisite files. All others rely on a
#'   generated string - either the empty string `""` when the checksums match,
#'   an error with the file names and checksums when they fail to match, or an
#'   error if something prevents them from being calculated (e.g. missing
#'   files). `as="check"` returns this string, `as="error"` and `as="stop"` are
#'   aliases; they either return the generated empty string (silently), or
#'   trigger an error exit if the string is not empty. `as="warning"` and
#'   `as="message"` return the generated string silently, but also signal the
#'   appropriate message if the string is not empty.
#'
#' @return Depends on `as=`. With matching checksums, will either be TRUE or the
#'   empty string `"` (possibly silently). If checksums don't match, it will
#'   either be FALSE, a string giving the files and their mismatched checksums
#'   (possibly silently), or an error will be triggered. If there is a problem
#'   calculating the checksums, e.g. a file does not exits, the result will
#'   either be "NA", a string describing the problem, or an error will be
#'   triggered.
#'
#' @export
checkFileSum <- function( file, algo= c("md5", "sha1", "crc32", "sha256", "sha512"),
                          checkFile= NULL,
                          as= c( "test", "check", "error", "warning", "message", "stop" )) {
   algo <- match.arg(algo)
   as <- match.arg(as)

   # Using message to signal error
   message = c()
   if (! file.exists( file )) {
      message = c( message, paste0( "Can't find file: \"", file, "\"." ))
   }
   if (is.null(checkFile)) {
      checkFile <- paste0( file, ".", algo )
      if (! file.exists( checkFile )) {
         # toggle checkFile from hidden <-> non-hidden and try again
         guessBaseName <- basename( checkFile )
         if (grepl("^[.]", guessBaseName)) {
            guessBaseName <- sub( "^[.]+", "", guessBaseName )
         }
         else {
            guessBaseName <- paste0( ".", guessBaseName)
         }
         checkFile <- file.path( dirname(checkFile), guessBaseName)
         if (! file.exists( checkFile )) {
            message = c( message, paste0( "Can't find a matching checksum file for: \"", file, "\"." ))
         }
      }
   }
   else if (! file.exists( checkFile )) {
      message = c( message, paste0( "Can't find specified checksum file: \"", checkFile, "\"." ))
   }

   message = paste0( message, collapse= "\n" )

   # Check message, if empty, was no error, so have file and checkFile.
   if ( nchar( message ) == 0 ) {
      got <-  digest::digest( object= file, algo=algo, file= TRUE )
      want <- readLines( checkFile, n=1 )
   }

   # as 'test' returns TRUE or FALSE, everything else requires a message
   if (as == "test" && nchar( message ) != 0 ) {
      return( NA )
   } else if (as == "test" && nchar( message ) == 0 ) {
      return( got == want )
   } else if (as != "test" && nchar( message ) == 0 ) {
      if (got != want) {
         message = paste0( "**Checksum '", algo, "' mismatch**:\n",
                           "\tFrom the checkFile: '", want, "' (", checkFile, "),\n",
                           "\tComputed: '", got, "' (", file, ").\n")
      }
   }

   # as determines if returning a string or signaling a condition. Conditions
   # are only signaled if there is a message. If the condition is not fatal,
   # the message string is returned invisibly.
   if ( as == "check" ) {
      return( message )
   } else if ( nchar( message ) != 0 ) {
      if ( as == "error" ) {
         stop( message )
      } else if ( as == "warning" ) {
         warning( message )
      } else if ( as == "message" ) {
         message( message )
      }
   }
   invisible( message )
}

#' Create a checksum file
#'
#' Calculates a checksum for a file and saves it to file that by default is
#' named the same, except with an appended extension matching the algorithm.
#' @param file The file to validate
#' @param algo The checksum algorithm to use (see `digest::digest()`).
#' @param checkFile The name of the checksum file to create.
#' By default named the same as file, with an additional extension matching
#' `algo`, e.g. for an md5 checkFile for `file.txt`, the default would be
#'  `file.txt.md5`. If this filename includes a directory component, it will
#'  be created if needed.
#' @param show Should the checkFiles be visible (i.e. NOT start with a ".")? This is
#' ignored if the checkFile is specified. Note, if the file is
#' hidden, then its default checksum file will be hidden regardless of this setting.
#' @param overwrite If the checkFile already exists and does not match, it is
#' an error unless this is set TRUE, in which case the file is just overwritten.
#' If it matches, then done (in this case modify timestamps will not be changed).
#'
#' @return The generated checkFile
#'
#' @export
saveFileSum <- function( file, algo= c("md5", "sha1", "crc32", "sha256", "sha512"),
                         checkFile= NULL, show= FALSE, overwrite= FALSE ) {
   algo <- match.arg(algo)
   if (is.null(checkFile)) {
      checkFile <- paste0( file, ".", algo )
      if (! show) {
         checkFileBase <- basename( checkFile )
         if (! grepl("^\\.", checkFileBase)) {
            checkFileBase <- paste0( ".", checkFileBase )
            checkFilePath <- dirname( checkFile )
            checkFile <- file.path( checkFilePath, checkFileBase)
         }
      }
   }
   if (! file.exists( file )) {
      stop( "Can't find file: \"", file, "\"." )
   }
   checkFilePath <- dirname( checkFile )
   if (! dir.exists(checkFilePath)) {
      dir.create( checkFilePath, recursive= TRUE )
   }

   # Handle existing checkfile
   if (file.exists( checkFile )) {

      # Good as is?
      okAsIs <- checkFileSum( file, algo= algo, checkFile= checkFile )

      if ( is.na(okAsIs) || ! okAsIs ) {
         # Nope. If I can't overwrite, I'm stuck.
         if (! overwrite) {
            stop( "Checksum file exists and does not match!",
                  " Without `overwrite= TRUE`, can't replace: \"", checkFile, "\"." )
         }
         # Not ok, but I can overwrite! I'll just pretend it doesn't exist.
      } else {
         # Yup. Just return filename
         return( checkFile )
      }
   }

   # Create checkFile. Will overwrite any existing file, but know its ok.
   got <-  digest::digest( object= file, algo=algo, file= TRUE )
   writeLines( got, checkFile )

   # Validate checkFile just written is correct
   wrote <- tryCatch( readLines( checkFile, n=1 ),
                      error = function(e) as.character( e ),
                      warning = function(e) as.character( e ))
   if (got != wrote) {
      stop( "Something appears to have gone wrong writing the checkFile.",
            " Can't verify: \"", checkFile, "\"." )
   }

   # Done, return checkFile
   checkFile
}

