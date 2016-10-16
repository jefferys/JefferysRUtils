#' Apply a function by block to a file or connection
#'
#' Applies a transforming or filtering function to blocks of lines from a file
#' or connection, processing each block as a character vector. Sequentially reads blocks
#' of \code{chunkSize} lines from the file or connection using \code{readLines},
#' calls the function \code{FUN} on each block, and returns a vector or list of
#' the combined results. This allows moderately efficient reading and processing
#' of large files, potentially larger than would fit in memory. With the
#' default setting of \code{chunkSize = -1L}, the whole file will be read in as
#' a single block.
#'
#' As this takes connections as well as file names, it can read from compressed
#' files, URLs, etc. As it can read in chunks, it can process files or text
#' streams larger than would fit in memory. Note that if the size of chunks
#' is too small this may be inefficient in both time and memory.
#'
#' @section File reading: The file is read with \code{\link{readLines}}; the
#'   \code{.} prefixed parameters are passed through to \code{readLines} and
#'   allow controlling how embedded nul characters are treated and if the blocks
#'   passed to \code{FUN} are annotated with encoding information.
#'
#'   Any of \code{LF}, \code{CRLF}, or \code{CR} are recognized as line
#'   separators. The elements of the character vector chunks passed to
#'   \code{FUN} do not contain these separators.
#'
#'   If a file has no terminal EOL marker, the last line will not be included if
#'   the file/connection was non-blocking and text-mode. Otherwise it will be
#'   included, but as this may signal truncated or incomplete data, a warning is
#'   generated.
#'
#'   Embedded nul characters will by default end their lines, but trigger a
#'   warning that this is happening. No text between the null and the next EOL
#'   marker (\code{LF}, \code{CRLF}, or \code{CR}) will be included in the line
#'   as returned. Turn off nul warnings if you expect null characters and want
#'   this truncation (set \code{.warn= FALSE}). To ignore nul characters and
#'   return all text in the line, dropping the nul characters, set
#'   \code{.skipNul= TRUE}. No warning is then generated.
#'
#' @section Kinds of functions: Only functions that preserve elements work
#'   simply. This means the function outputs results where each element of the
#'   output is based on only one element of the input. For example, \code{out <-
#'   nchar(in)} is a 1-1 vector transform of \code{in}, so it just works.
#'   \code{out <- intersect(in, setVec)} also works, as on an element by element
#'   basis, it either outputs or does not output an element from \code{in}.
#'   \code{grep} works similarly with \code{value=TRUE}. Functions that return
#'   indices may not be useful unless \code{filter= TRUE} is set as these
#'   indices wii be relative to the chunk boundaries. If you want to use other
#'   kinds of functions, you can always set unlist=FALSE and manually combine
#'   the results yourself.
#'
#'   Note: Applying your function to blocks with this function will likely be
#'   faster than applying it one line at a time with \code{fileLineApply}.
#'
#' @param con The path to a text file to read, or a \code{\link{connection}}
#'   (including urls). Will auto-detect compression and read single gzip,
#'   bzip2, zip, and xz/lzma files or connections to them (i.e. no tarred files
#'   or zipped directories allowed).
#'
#'   Open connections are read from their current positions. If not already
#'   open, a connection will be opened in "rt" mode and then closed again before
#'   the call to this function returns.
#'
#' @param FUN The function called on each file block (vector of file lines). This
#'   function should take a character vector of characters as its first parameter, or as
#'   the first unsupplied named parameter (see "grep" example). Only some functions
#'   will work well, see Kinds of functions
#'
#' @param ... Additional arguments to pass to the \code{FUN}.
#'
#' @param chunkSize The maximum number of lines to read from a file in one
#'   chunk. \code{FUN} is called on each chunk separately and then the
#'   results are combined. All lines in the last chunk will be read,
#'   even though there are probably less than chunkSize lines left. The last
#'   line may, on rare occasions, not be read (this will trigger a warning. See
#'   "File reading", below.)
#'
#' @param unlist Internally, the results from each block is saved separately in
#'   a list element. By default these are flattened before returning to hide the
#'   file chunking. To preserve this split by block, set \code{unlist= FALSE}.
#'
#' @param filter By default the results from the function call are returned. To
#'   use a function to select lines from the file, set this \code{TRUE}.
#'   \code{FUN} must then return a vector of numeric or logical values. Note
#'   that it may be faster to select lines directly with value returning
#'   functions like \code{grep(value= TRUE)}.
#'
#' @param growStart Tuning parameter for internal memory management. The initial
#'   number of blocks allowed before reallocating space. By default this is
#'   10,000.
#'
#' @param growX Tuning parameter for internal memory management. If run out of
#'   space to store block results, increase total space by \code{growX * current
#'   + growAdd}. Done each time run out of space, allocating successively large
#'   blocks of space.
#'
#' @param growAdd Tuning parameter for internal memory management. If run out of
#'   space to store block results, increase total space by \code{growX * current
#'   + growAdd}. Done each time run out of space, allocating successively large
#'   blocks of space. By default this is 0.
#'
#' @param .warn Passed through to readLines as the \code{warn}
#'   parameter. By default every embedded nul triggers a warning. Can set this
#'   to FALSE if don't want a warning. Note, no warning is generated regardless
#'   of this flag when skipping nuls (i.e. when \code{.skipNul= TRUE}).
#'
#' @param .skipNul Passed through to readLines as the \code{skipNul}
#'   parameter, By default every embedded nul terminates a line (and will then
#'   also trigger a warning by default, when \code{.warn= TRUE}). Setting this
#'   TRUE keeps reading past the null up to EOL, and does not warn.
#'
#' @param .encoding Passed through to readLines as the \code{encoding}
#'   parameter.
#'
#' @return The result of applying \code{FUN} to each chunk in the file or
#'   connection, as returned by \code{\link{readLines}}. It is probably
#'   invisible, but the internal process is to assign the results from each
#'   block to successive list elements, and then unlist one level before
#'   returning.
#'
#' @examples
#' ### Tiny file to parse (auto-deleted on exit)
#' content <- c( "One line", "Two lines.", "", "Four" )
#' fileName <- tempfile()
#' writeLines(content, fileName)
#'
#' ### Transformation function
#' fileBlockApply( fileName, "nchar" )
#' #> [1]  8 10  0  4
#'
#' ### Selecting functions
#' fileBlockApply( fileName, "intersect", c("", "One line") )
#' #> [1] "One line" ""
#'
#' fileBlockApply( fileName, "grep", pattern="line", value=TRUE )
#' #> [1] "One line"   "Two lines."
#'
#' ### Keeping the block structure
#' # (chunkSize is unreasonably tiny to show block boundary)
#' fileBlockApply( fileName, "nchar", chunkSize=2, unlist=FALSE )
#' #> [[1]]
#' #> [1]  8 10
#' #>
#' #> [[2]]
#' #> [1] 0 4
#'
#' ### Explicit function definition
#' # Faking a file connection
#' values <- as.character(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#' con <- textConnection( values )
#'
#' # Filtering manually
#' fileBlockApply(con, function (x) { y <- as.numeric(x); y[y > 5] }, chunkSize= 4)
#' #> [1]  6  7  8  9 10
#'
#' # Rewind fake connection
#' con <- textConnection( values )
#'
#' fileBlockApply( con, function (x) { y <- as.numeric(x); y[y > 5] },
#'                 chunkSize= 4, unlist= FALSE )
#' #> [[1]]
#' #> numeric(0)
#' #>
#' #> [[2]]
#' #> [1] 6 7 8
#' #>
#' #> [[3]]
#' #> [1]  9 10
#'
#' ### Manually merging block results (counts lines)
#' con <- textConnection( values )
#' blockLengths <- fileBlockApply( con, "length", chunkSize= 4)
#' totalLength <- sum(blockLengths)
#'
#' con <- textConnection( values )
#' sum( fileBlockApply( con, function (x) sum(as.integer(x)), chunkSize= 4))
#' #> 55
#'
#' ### Filtering
#' # A logical returning function
#' fileBlockApply(con, function (x) { as.numeric(x) > 5 }, chunkSize= 4)
#' #> [1] FALSE FALSE FALSE FALSE FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
#'
#' # Filter returns original lines:
#' con <- textConnection( values )
#' fileBlockApply(con, function (x) { as.numeric(x) > 5 }, chunkSize= 4, filter= TRUE)
#' #> [1] "6"  "7"  "8"  "9"  "10"
#'
#' # An index returning function (relative to block start)
#' con <- textConnection( values )
#' fileBlockApply( con, function (x) { length(x) }, chunkSize= 3)
#' #> [1] 3 3 3 1
#'
#' # Filter using index, returns last line of each block,
#' # including uneven block at end
#' con <- textConnection( values )
#' fileBlockApply( con, function (x) { length(x) }, chunkSize= 3, filter=TRUE)
#' #> [1] "3"  "6"  "9"  "10"
#'
#' @export
fileBlockApply <- function( con, FUN, ...,
   chunkSize= -1L, unlist=TRUE, filter=FALSE,
   growStart= 1e5, growX= 2, growAdd= 0,
   .warn= TRUE, .skipNul= FALSE, .encoding= "unknown"
) {
   if (chunkSize == 0) { stop( "chunkSize may not be 0." ) }
   if (growStart < 1) { stop( "growStart must be >= 1." ) }
   if (growX < 1) { stop( "growX must be >= 1." ) }
   if (growAdd < 0) { stop( "growAdd must be >= 0." ) }
   if (growX == 1 && growAdd ==0) { stop( "must have growX > 1 or growAdd > 0." )}

   if (is.character(con)) {
      con <- file(con)
   }
   # Open connection if not open
   if (! isOpen(con)) {
      open(con,"rt")
      on.exit( close(con), add= TRUE )
   }

   # Pre-allocate space for returned values, one value per chunk
   growMe <- list()
   length(growMe) <- growStart

   # Loop once per chunk until all processed
   k <- 0
   done <- FALSE
   while( ! done ) {

      # Read a chunk of the file, position is remember from read to read
      # as connection is open when called.
      chunk <- readLines( con, n=chunkSize,
                          warn=.warn, encoding= .encoding, skipNul= .skipNul )

      # Last chunk will be smaller, or 0. Default chunksize = -1L always fails.
      if (length(chunk) != chunkSize) {
         done <- TRUE
      }

      # If fits exactly, last chunk is length 0, so do nothing.
      if (length(chunk) != 0) {
         # Count chunks
         k <- k + 1

         # If haven't got enough space to save another chunks result in the
         # list, grow the list by some chunk (not just 1 position).
         if (k > length(growMe)) {
            length(growMe) <- length(growMe) * growX + growAdd
         }

         # Finally, apply function to this chunk and save result
         paramList <- list(chunk, ...)

         if (filter) {
            growMe[[k]] <- chunk[do.call(FUN, paramList)]
         }
         else {
            growMe[[k]] <- do.call(FUN, paramList)
         }
      }
   }

   # Trim unallocated (NULL) space from list of results
   length(growMe) <- k

   # Unlist one level to stitch results to match what would normally be output.
   if (unlist) {
      return(unlist(growMe, recursive= FALSE))
   }
   else {
      return(growMe)
   }
}


#' Apply a function to a file or connection
#'
#' Applies a function to each line of a file or connection, processing each line
#' as an element of a character vector. Sequentially reads blocks of
#' \code{chunkSize} lines from the file or connection using \code{readLines},
#' \code{sapply(USE.NAMES=FALSE)}ing the \code{FUN} across each block and
#' returning the combined result from all blocks. This implementation allows
#' moderately efficient reading and processing of large files, potentially
#' larger than would fit in memory. With the default setting of \code{chunkSize
#' = -1L}, the whole file will be read in as a single block.
#'
#' As this takes connections as well as file names, it can read from compressed
#' files, URLs, etc. As it can read in chunks, it can process files or text
#' streams larger than would fit in memory. Note that if the size of chunks
#' is too small this may be inefficient in both time and memory.
#'
#' @section File reading: The file is read with \code{\link{readLines}}; the
#'   \code{.} prefixed parameters are passed through to \code{readLines} and
#'   allow controlling how embedded nul characters are treated and if the blocks
#'   passed to \code{FUN} are annotated with encoding information.
#'
#'   Any of \code{LF}, \code{CRLF}, or \code{CR} are recognized as line
#'   separators. The elements of the character vectors (file lines) passed to
#'   \code{FUN} do not contain these separators.
#'
#'   If a file has no terminal EOL marker, the last line will not be included if
#'   the file/connection was non-blocking and text-mode. Otherwise it will be
#'   included, but as this may signal truncated or incomplete data, a warning is
#'   generated.
#'
#'   Embedded nul characters will by default end their lines, but trigger a
#'   warning that this is happening. No text between the null and the next EOL
#'   marker (\code{LF}, \code{CRLF}, or \code{CR}) will be included in the line
#'   as returned. Turn off nul warnings if you expect null characters and want
#'   this truncation (set \code{.warn= FALSE}). To ignore nul characters and
#'   return all text in the line, dropping the nul characters, set
#'   \code{.skipNul= TRUE}. No warning is then generated.
#'
#' @section Kinds of functions: If a function takes a vector of characters as
#'   its input, it might be more efficient to use \code{fileBlockApply}. This is
#'   especially true for filtering functions, like grep. Functions that return
#'   indices or logicals vectors can be used with \code{fileFilterApply} which
#'   will return the elements instead of an index or logical vector.
#'
#' @param con The path to a text file to read, or a \code{\link{connection}}
#'   (including urls). Will auto-detect compression and read single gzip,
#'   bzip2, zip, and xz/lzma files or connections to them (i.e. no tarred files
#'   or zipped directories allowed).
#'
#'   Open connections are read from their current positions. If not already
#'   open, a connection will be opened in "rt" mode and then closed again before
#'   the call to this function returns.
#'
#' @param FUN The function called on each file line (character vector element).
#'   This function should take a (1 element) character vector of characters as
#'   its first parameter, or as the first unsupplied named parameter. If the
#'   function takes a vector of characters, it is probably better to use
#'   \code{fileBlockApply}
#'
#' @param ... Additional arguments to pass to the \code{FUN}.
#'
#' @param chunkSize The maximum number of lines to read from a file in one
#'   chunk. \code{FUN} is \code{sapply}ed to each chunk separately and then the
#'   results are combined. All lines in the last chunk will be read,
#'   even though there are probably less than chunkSize lines left. The last
#'   line may, on rare occasions, not be read (this will trigger a warning. See
#'   "File reading", below.)
#'
#' @param unlist Internally, the results from each block is saved separaetly in
#'   a list element. By default these are flattened before returning to hide the
#'   file chunking. To preserve this list by block, set \code{unlist= FALSE}.
#'
#' @param filter By default the function result is returned. If a function
#'   returns a logical value, this can be used to select lines by setting this
#'   parameter \code{TRUE}. When filtering, the parameters \code{.simplify} and
#'   \code{.USE.NAMES} are ignored. Note: unlike \code{fileBlockApply}, an
#'   integer returning function is not allowed.
#'
#' @param growStart Tuning parameter for internal memory management. The initial
#'   number of blocks allowed before reallocating space. By default this is
#'   10,000.
#'
#' @param growX Tuning parameter for internal memory management. If run out of
#'   space to store block results, increase total space by \code{growX * current
#'   + growAdd}. Done each time run out of space, allocating successively large
#'   blocks of space. By default this is 2.
#'
#' @param growAdd Tuning parameter for internal memory management. If run out of
#'   space to store block results, increase total space by \code{growX * current
#'   + growAdd}. Done each time run out of space, allocating successively large
#'   blocks of space. By default this is 0.
#'
#' @param .warn Passed through to readLines as the \code{warn}
#'   parameter. By default every embedded nul triggers a warning. Can set this
#'   to FALSE if don't want a warning. Note, no warning is generated regardless
#'   of this flag when skipping nuls (i.e. when \code{.skipNul= TRUE}).
#'
#' @param .skipNul Passed through to readLines as the \code{skipNul}
#'   parameter, By default every embedded nul terminates a line (and will then
#'   also trigger a warning by default, when \code{.warn= TRUE}). Setting this
#'   TRUE keeps reading past the null up to EOL, and does not warn.
#'
#' @param .encoding Passed through to readLines as the \code{encoding}
#'   parameter.
#'
#' @param .simplify Passed through to sapply as the \code{simplify} parameter.
#' Set FALSE to avoid flattening the results for a block into a vector or array.
#'
#' @param .USE.NAMES Passed through to sapply as the \code{simplify} parameter.
#' Set TRUE to keep the source line from the file as the name of the returned value.
#'
#' @return The result of applying \code{FUN} to each line in the file or
#'   connection, as returned by \code{\link{readLines}}. It is probably
#'   invisible, but the internal process is to assign the results from each
#'   block to successive list elements, and then unlists one level before
#'   returning. You can always unlist the result to get a vector, depending
#'   on what \code{FUN} returns
#'
#' @examples
#' ### Tiny file to parse (auto-deleted on exit)
#' content <- c( "One line", "Two lines.", "", "Four" )
#' fileName <- tempfile()
#' writeLines(content, fileName)
#'
#' ### Transformation function
#' fileLineApply( fileName, "toupper" )
#' #> [[1]]
#' #> [1] "ONE LINE"
#' #>
#' #> [[2]]
#' #> [1] "TWO LINES."
#' #>
#' #> [[3]]
#' #> [1] ""
#' #>
#' #> [[4]]
#' #> [1] "FOUR"
#'
#' unlist(fileLineApply( fileName, "toupper" ))
#' #> [1] "ONE LINE"   "TWO LINES." ""           "FOUR"
#'
#' ### Selecting functions
#' fileLineApply( fileName, "intersect", c("", "One line") )
#' #> [[1]]
#' #> [1] "One line"
#' #>
#' #> [[2]]
#' #> character(0)
#' #>
#' #> [[3]]
#' #> [1] ""
#' #>
#' #> [[4]]
#' #> character(0)
#'
#' unlist(fileLineApply( fileName, "intersect", c("", "One line") ))
#' #> [1] "One line" ""
#'
#' ### Keeping the block structure
#' # (chunkSize is unreasonably tiny to show block boundary)
#' fileLineApply( fileName, "nchar", chunkSize=3, unlist=FALSE )
#' #> [[1]]
#' #> [[1]][[1]]
#' #> [1] 8
#' #>
#' #> [[1]][[2]]
#' #> [1] 10
#' #>
#' #> [[1]][[3]]
#' #> [1] 0
#' #>
#' #>
#' #> [[2]]
#' #> [[2]][[1]]
#' #> [1] 4
#'
#' # Only need to unlist once, as recursive=TRUE is the default.
#' unlist(fileLineApply( fileName, "nchar", chunkSize=3, unlist=FALSE ))
#' #> [1]  8 10  0  4
#'
#' ### Explicit function definition - word count by line
#' # Manually for a normal file (sapply simplifies)
#' lengths( strsplit(readLines(fileName), "\\s+"))
#' #> [1] 2 2 0 1
#'
#' # Using file apply (with tiny chunkSize)
#' fileLineApply( fileName, function (x) { lengths(strsplit(x, "\\s+")) }, chunkSize= 2)
#' #> [1] 2 2 0 1
#'
#' # A logical returning function
#' fileLineApply( fileName, function (x) { lengths(strsplit(x, "\\s+")) == 1 },
#'                chunkSize= 2 )
#' #> [1] FALSE FALSE FALSE  TRUE
#'
#' # Filtering on a logical returning function
#' fileLineApply( fileName, function (x) { lengths(strsplit(x, "\\s+")) == 1 },
#'                chunkSize= 2, filter=TRUE )
#' #> [1] "Four"
#'
#' @export
fileLineApply <- function( con, FUN, ...,
   chunkSize= -1L, unlist=TRUE, filter=FALSE,
   growStart= 1e5, growX= 2, growAdd= 0,
   .warn= TRUE, .skipNul= FALSE, .encoding= "unknown",
   .simplify=TRUE, .USE.NAMES=FALSE
) {
   if (chunkSize == 0) { stop( "chunkSize may not be 0." ) }
   if (growStart < 1) { stop( "growStart must be >= 1." ) }
   if (growX < 1) { stop( "growX must be >= 1." ) }
   if (growAdd < 0) { stop( "growAdd must be >= 0." ) }
   if (growX == 1 && growAdd ==0) { stop( "must have growX > 1 or growAdd > 0." )}

   if (is.character(con)) {
      con <- file(con)
   }
   # Open connection if not open
   if (! isOpen(con)) {
      open(con,"rt")
      on.exit( close(con), add= TRUE )
   }

   # Pre-allocate space for returned values, one value per chunk
   growMe <- list()
   length(growMe) <- growStart

   # Loop once per chunk until all processed
   k <- 0
   done <- FALSE
   while( ! done ) {

      # Read a chunk of the file, position is remember from read to read
      # as connection is open when called.
      chunk <- readLines( con, n=chunkSize,
                          warn=.warn, encoding= .encoding, skipNul= .skipNul )

      # Last chunk will be smaller, or 0. Default chunksize = -1L always fails.
      if (length(chunk) != chunkSize) {
         done <- TRUE
      }

      # If fits exactly, last chunk is length 0, so do nothing.
      if (length(chunk) != 0) {
         # Count chunks
         k <- k + 1

         # If haven't got enough space to save another chunks result in the
         # list, grow the list by some chunk (not just 1 position).
         if (k > length(growMe)) {
            length(growMe) <- length(growMe) * growX + growAdd
         }

         # Finally, apply function to this chunk and save result
         if (filter) {
            growMe[[k]] <- chunk[sapply( chunk, FUN, ... )]
         }
         else {
            growMe[[k]] <- sapply(chunk, FUN, ..., simplify=.simplify, USE.NAMES=.USE.NAMES)
         }
      }
   }

   # Trim unallocated (NULL) space from list of results
   length(growMe) <- k

   # Unlist one level to stitch results to match what would normally be output.
   if (unlist) {
      return(unlist(growMe, recursive= FALSE))
   }
   else {
      return(growMe)
   }
}