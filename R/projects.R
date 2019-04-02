#' Make a directory for results.
#'
#' @param base The base directory to create a results directory in. Will be
#'   created (recursively) if needed, unless `rerun= TRUE` is set. May be
#'   absolute or relative.
#' @param dir The results directory under `base=` to be created (recursively).
#'   Must not exist unless `rerun= TRUE` is set. Must be relative to `base=`.
#' @param rerun Set `TRUE` to reuse an existing run directory; it must already
#'   exist.
#'
#' @return The path to the created (or existing) output directory,
#'   `<base>/<outDir>`.
#'
#' @examples
#' baseDir <- tempfile()
#' outDirName <- "run-1"
#' dir.exists( file.path( baseDir, outDirName )) == FALSE
#' myOutDir <- useResultsDir( baseDir, outDirName )
#' dir.exists( file.path( baseDir, outDirName )) == TRUE
#' useResultsDir( baseDir, outDirName, rerun=TRUE ) == myOutDir
#'
#' @export
useResultsDir <- function( base, dir, rerun= FALSE ) {

   path <- file.path( base, dir )
   if (rerun) {
      if (! dir.exists( path )) {
         stop( sprintf(
            paste( sep="\t",
               "Aborting: Dir not found: \"%s\".",
               "On rerun, output directory path must exist.\n"
            ), path
         ))
      }
      else {
         warning( sprintf(
            "Warning - Rerun is reusing existing directory: \"%s\".\n",
            path
         ))
      }
   } else {
      if (dir.exists( path )) {
         stop( sprintf(
            paste( sep="\t",
                   "Aborting: Output dir already exists: \"%s\".",
                   "Set rerun= TRUE to overwrite exising output.\n"
            ), path
         ))
      }
      else {
         if (! dir.exists( base )) {
            ok <- dir.create( base, recursive= TRUE, showWarnings= TRUE )
            if (! all( ok )) {
               stop( sprintf(
                  "Aborting: Error creating base directory: \"%s\".\n",
                  base
               ))
            }
         }
         ok <- dir.create( path, recursive= TRUE, showWarnings= TRUE )
         if (! all( ok )) {
            stop( sprintf(
               "Aborting: Error creating output directory: \"%s\".\n",
               path
            ))
         }
      }
   }
   return( path )
}