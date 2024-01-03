
#' Get the SHA256 checksum of a file.
#'
#' Wrapper around `openssl::sha256()` to make it easy to calculate a checksum of a
#' file. Converts the returned hash code to a string by default, but can set
#' `as.hash= TRUE` to get the unconverted raw byte based hash object instead.
#'
#' @param file The path to a file to checksum
#' @param as.hash Set TRUE to return the hash byte object instead of converting
#'   to a string
#'
#' @return Returns the hash as a string, unless as.hash= TRUE is set, in
#'   which case the hash byte object as obtained from `openssl::sha256()` is
#'   returned.
#'
#' @examples
#' aFile <- JefferysRUtils::makeTempFile( c( "one fish", "two fish" ))
#' aHash <- sha256( aFile )
#' aHash
#' #> "a5fb5357754cb5a6ae85090aed325435388c8b0d6e295a058bab55102ffa188e"
#'
#' aHashObj <- sha256( aFile, as.hash= TRUE )
#' class(aHashObj)
#' #> [1] "hash"   "sha256"
#' str(aHashObj)
#' #> 'hash' raw [1:32] a5 fb 53 57 ...
#'
#' @export
sha256 <- function( file, as.hash= FALSE ) {
   if (! file.exists( file )) {
      stop( "No such file: ", file )
   }
   hash <- openssl::sha256( file( file ))
   if (! as.hash) {
      hash <- as.character( hash )
   }
   hash
}


#' Validate files by SHA256 checksum hashing
#'
#' Validates files by calculating their SHA256 checksums and comparing
#' them to the expected hash. Returns a named boolean vector indicating matching
#' or mismatching files, by path. `error` can be set to return `FALSE` or `NA`
#' for problem files instead of throwing an error. File names can be any mix of
#' absolute and relative paths, where relative is to the current working directory.
#'
#' The expected `checksums` hash strings for files can be provided as a list or vector and can be
#' named. The files specified via `files` will be validated using
#' these checksums. If named, all checksums must be named for the path to the
#' file they validate. They are treated like a
#' "dictionary" from which the expected checksum of a file can be looked up, by path. If
#' unnamed, then checksums must be in the same order and have the same length
#' as the provided `files` vector.
#'
#' If `checksums` are not specified, then for each file in `files`, a
#' "note" file containing the string checksum as its only content (EOL
#' optional) will be looked for. They are expected to be named the same as the
#' `files`, but with the addition of the extension `ext` (by default `.sha256`).
#' It is an error if an expected `.sha256` note file is not found, but this behavior
#' can be configured by setting `error`.
#'
#' @param checksums A vector or list of expected checksums. If named, they will
#'   be treated as a dictionary of hash values. If unnamed, file
#'   names must be provided in parallel in `files`. This is optional when note
#'   files identified by `ext` are being used to store checksum values. It is an
#'   error if only some checksums have names.
#' @param files The files to check as a string vector. If `checksums` are not
#'   provided, note files containing the checksum will looked for based on
#'   `ext`. If `checksums` are unnamed, `files` must match in order and length.
#'   When `checksums` are named, `files` is optional and will default to the
#'   checksum names.
#' @param ext If only files are specified and checksums are not, checksums are
#'   expected to be provided in "note" files stored on the file system in
#'   parallel to the files being validated. For every file to check, the name of
#'   the matching "note" file is obtained by suffixing this extension to the
#'   name of the file it matches. The note file should contain the checksum of
#'   the matching file as a string. The hash string should be the only thing
#'   in the file. It doesn't matter if the checksum string ends with an EOL or
#'   not. This generally should start with a "." and by default will be `.sha256`.
#' @param error Boolean value. `NA` or `FALSE` specifies the result to return
#'   when files are missing (either files to be validated or expected note
#'   files). Setting this `TRUE` causes an error to be thrown when a file is
#'   not found, halting checking.
#'
#' @return A boolean vector indicating if files match their
#' provided SHA256 checksum hashes.
#'
#' @examples
#'
#' @export
checkSha256 <- function( checksums=NULL, files= NULL, error= NA, ext=".sha256") {

      # Do hashes come from note files?
      if ( is.null( checksums )) {
         if ( is.null( files )) {
            stop( "Must specify at least one of 'checksums' or 'files'.")
         }

         getNoteFileVal <- function( file, error, ext ) {
            # Guess file name as a suffixed extension
            sha256File <- file %p% ext
            if (! file.exists( sha256File )) {
               # Not added extension, so guess as replaced extension
               sha256File <- sub( "[.][^.]$", ext, file )
               if (! file.exists( sha256File )) {
                  return( ifElseMore( error, true= stop(
                     "Can't find checksum note file for \"", file,
                     "\" using extension \"", ext, "\"."
                  )))
               }
            }

            sha256 <- readLines( sha256File, n= 1, warn= FALSE)
            if (nchar(sha256 != 64)) {
               return( ifElseMore( error, stop(
                  "Not a SHA256 checksum file: \"", sha256File, "\"."
               )))
            return( sha256 )
         }
      }
   }

   (sha256 == sha256( file ))
}

#' Download a file from GitHub
#'
#' Given the file name, repository, and commit/tag/branch, downloads the
#' specified (public) file from GitHub unless it already exists or is forced.
#' Does not support private repositories or directories.
#'
#' @param file The name of the file to download
#' @param repo The repository to download from, e.g. "Jefferys/JefferysRUtils"
#' @param commit The commit, tag, or branch to download.
#' @param dir The directory to download to, by default the current working directory.
#' @param force Download and over-write if already exists in output directory.
#' @param sha256 Expected SHA256 checksum.
#' @param ... Other parameters to pass through to download.file
#'
#' @return The exit code from download.file - 0 for success, positive integer
#' for error, and NA if not attempted due to an existing file.
#'
#' @examples
#' \dontrun{
#' githubGet( "README.Rmd", repo= Jefferys/JefferysRUtils,
#'            commit= 'e5dab82f022afa311be9d078d3e080fb88a2cfaf' )
#' githubGet( "README.Rmd", repo= Jefferys/JefferysRUtils,
#'            commit= 'dev' )
#' }
#' @export
githubGet <- function( file, repo, commit, dir= "./", force= FALSE,
                       sha256= NULL, ... ) {
   githubUrl <- paste0(paste( "https://github.com",
                              repo, "blob", commit, file, sep= "/" ), "?raw=true")
   baseFile <- gsub( ".*/", "", file, perl= TRUE )
   toPath <- file.path( dir, baseFile )

   # Default if don't try.
   retVal <- NA_integer_
   if ( force || ! file.exists(toPath) ) {
      retVal <- utils::download.file( githubUrl, toPath, ... )
   }
   if (! is.null(sha256)) {
      hash <- sha256( toPath )
      if (as.character(sha256) != hash) {
         if (is.na(retVal)) {
            stop( "File already existed (not downloaded)",
                  " but SHA256 does not match.")
         }
         else {
            stop( "File as downloaded does not match provided SHA256." )
         }
      }
   }
   invisible( retVal )
}
