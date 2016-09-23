# utils.R - Utility functions
#
# Probably want to collect these into a separate package at some point.

#' Merge two lists
#'
#' Merges two lists, appending the second to the first. Expects that all
#' elements are named and that within a list, all names are unique. If the same
#' name is used in both lists, the element (value) from the second list is kept,
#' the one from the first list is dropped. That results in the order of elements
#' in the merged list being different. If \code{keepOrder = TRUE} is set, then
#' the order of elements in the first list is kept with the duplicated elements
#' from the second dropped.
#'
#' @param x A list
#' @param y Another list
#' @param keepOrder How to order list when dropping duplicate elements from
#'   \code{y} If FALSE, the default, just drops elements from \code{x} with the
#'   same name as an element in \code{y}, before appending \code{y}. If TRUE,
#'   preserves the order of element names as much as possible, by replacing the
#'   value of duplicated element names with those from \code{y}, then appending
#'   \code{y} with duplicated elements dropped.
#'
#' @return A list consisting of all elements from \code{x} not also in \code{y},
#'   and then all elements in \code{y}, or
#' @export
merge.list <- function(x, y, keepOrder= FALSE) {
   # x is list by dispatch
   if ( ! is.list( x )) stop( "Can't merge lists; 'x' is not a list." )
   if ( ! is.list( y )) stop( "Can't merge lists; 'y' is not a list." )
   if ( any( names(x) == "" )) stop( "'x' contains elements without names.")
   if ( any( names(y) == "" )) stop( "'y' contains elements without names.")
   if ( anyDuplicated( names(x) )) stop( "'x' contains elements with duplicated names.")
   if ( anyDuplicated( names(y) )) stop( "'y' contains elements with duplicated names.")
   if (length(y)  == 0) {return(x)} # Even when x is length 0!
   if (length(x)  == 0) {return(y)}
   if (! keepOrder) {
      dropX <- names(x) %in% names(y)
      return( append( x[! dropX], y))
   }
   else {
      dropY <- names(y) %in% names(x)
      replaceX <- names(y)[dropY]
      x[replaceX] <- y[replaceX]
      return( append( x, y[! dropY]))
   }
}

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

#' Log to multiple loggers (with different thresholds)
#'
#' These are essentially the same as the level-specific loggers in
#' \code{\link{futile.logger}}, except they each can take a vector of logger
#' names. The \code{\link{futile.logger}} package makes it easy to log, and allows
#' logging to both a file and the console, but not with different thresholds.
#' The \code{say\var{Level}} loggers described here support this as the default
#' case, but also support any combination of one or more loggers, possibly
#' differing at each point where a message is logged.
#'
#' Whether or not a message is propagated depends on each logger's level
#' separately. If only one name is specified, this is essentially identical to a
#' \code{flog.\var{level}} call. If no names are provided, by default will log
#' to two loggers: \code{"\var{package}.file"} and
#' \code{"\var{package}.console"}.
#'
#' All loggers should be defined before being used in a log message. Since
#' loggers are a hierarchical namespace based on the "." separator in the names,
#' \emph{each} undefined logger passed in causes a logging call to search up the
#' tree of loggers looking for a "fall-back" logger. As a last resort, this will
#' be logged by the root logger. This prevents messages being lost, but can
#' result in the same message being logged multiple times.
#'
#' To prevent a logger from logging anything, set its threshold to \code{OFF}.
#' \code{OFF} is exported as another constant for use with loggers and is a
#' higher priority than \code{FATAL} No logging function can generate a message
#' with priority greater than \code{FATAL} Note, this is not completely
#' consistent with the use of the other constants, where the string can be used.
#' \code{"INFO"} and \code{INFO} are treated similarly in most cases, whereas
#' \code{"OFF"} and \code{OFF} are not. Always use the constant \code{OFF} (i.e.
#' the value 0) and not the string.
#'
#' To ensure a logger logs everything, set its logging threshold to \code{"TRACE"}. No
#' logging function can generate a message with priority lower than \code{TRACE}
#'
#' @param msg The message to print, possibly a string with \code{\link{sprintf}}
#'   symbols (like \code{\%s}), unless \code{capture= TRUE}
#' @param name The logger names to use (a vector of strings). By default will
#'   use \code{c( \var{package}.file, \var{package}.code )}. A message of the
#'   appropriate level with be logged to each.
#' @param ... Extra arguments for use by \code{msg}, if it is a sprintf message
#'   (\code{capture= FALSE}), or objects to have the logger format and print
#'   starting on the line following a non-sprintf message. (\code{capture=
#'   TRUE}.)
#' @param capture By default (FALSE), extra object arguments are handled as
#'   format variables for the message, which should contain a matching number of
#'   sprintf variable symbols. If TRUE, then extra object arguments are handled
#'   by the appender layout, by default printed on the next line following
#'   \code{msg}.
#'
#' @return Returns the message from the last logger named, may be NULL if that
#'   logger does not log the message due to threshold issues and
#'   \code{flog.carp(TRUE)} is not set.
#'
#' @name sayInfo
#' @import futile.logger
NULL

#' @describeIn sayInfo Print a message to specified loggers with
#'   a threshold of \code{TRACE}.
#' @export
sayTrace <- function( msg, ...,
   name= paste0( packageName(), c( ".file", ".console" )), capture= FALSE
) {
   for( logger in name) {
      x <- flog.trace( msg= msg, ..., name= logger, capture= capture )
   }
   return(x)
}

#' @describeIn sayInfo Print a message to specified loggers with
#'   a threshold of \code{DEBUG} or \code{TRACE}.
#' @export
sayDebug <- function( msg, ...,
   name= paste0( packageName(), c( ".file", ".console" )), capture= FALSE
) {
   for( logger in name) {
      x <- flog.debug( msg= msg, ..., name= logger, capture= capture )
   }
   return(x)
}

#' @describeIn sayInfo Print a message to specified loggers with
#'   a threshold of \code{INFO}, \code{DEBUG} or \code{TRACE}.
#' @export
sayInfo <- function( msg, ...,
   name= paste0( packageName(), c( ".file", ".console" )), capture= FALSE
) {
   for( logger in name) {
      x <- flog.info( msg= msg, ..., name= logger, capture= capture )
   }
   return(x)
}

#' @describeIn sayInfo Print a message to specified loggers with
#'   a threshold of \code{WARN}, \code{INFO}, \code{DEBUG} or \code{TRACE}.
#' @export
sayWarn <- function( msg, ...,
   name= paste0( packageName(), c( ".file", ".console" )), capture= FALSE
) {
   for( logger in name) {
      x <- flog.warn( msg= msg, ..., name= logger, capture= capture )
   }
   return(x)
}

#' @describeIn sayInfo Print a message to specified loggers with a threshold of
#'   \code{ERROR}, \code{WARN}, \code{INFO}, \code{DEBUG} or \code{TRACE}.
#' @export
sayError <- function( msg, ...,
   name= paste0( packageName(), c( ".file", ".console" )), capture= FALSE
) {
   for( logger in name) {
      x <- flog.error( msg= msg, ..., name= logger, capture= capture )
   }
   return(x)
}

#' @describeIn sayInfo Print a message to specified loggers with a threshold of
#'   \code{FATAL} \code{ERROR}, \code{WARN}, \code{INFO}, \code{DEBUG} or
#'   \code{TRACE}.
#' @export
sayFatal <- function( msg, ...,
   name= paste0( packageName(), c( ".file", ".console" )), capture= FALSE
) {
   for( logger in name) {
      x <- flog.fatal( msg= msg, ..., name= logger, capture= capture )
   }
   return(x)
}

#' @export
OFF <- c(OFF= 0L)

#' Initialize default loggers
#'
#' Sets up the default \code{"\var{package}.console"} and
#' \code{"\var{package}.file"} loggers. Use the \code{say\var{level}} function
#' to log simultaneously to them. If you log from a script or a console and
#' don't specify the log file, it will write to the probably hidden file
#' \file{".log"}. Allowed log levels are, in order of decreasing importance:
#' \code{OFF}, \code{FATAL}, \code{ERROR}, \code{WARN}, \code{INFO},
#' \code{DEBUG}, \code{TRACE}.
#'
#' @param file The name of the log file, by default \file{"\var{package}.log"}
#'   where \var{package} is guessed using \code{link{packageName()}}.
#' @param fileLevel Only messages at least this important will be saved to the
#'   log file, by default "WARN".
#' @param fileLevel Only messages at least this important will be printed to the
#'   console, by default "INFO".
#' @return Nothing, called only for its side effect of initializing loggers.
#'
#' @examples
#' \dontrun{
#'   initSayLoggers()
#'   # Won't be logged
#'   sayDebug('Initialized logging.')
#'   # Logged only to console, not file
#'   sayInfo('Welcome!')
#'   # Logged to console and file.
#'   sayWarn("This conversation is being monitored.")
#' }
#' @export
initSayLoggers <- function( file= packageName() %p% ".log",
                            fileLevel= WARN, consoleLevel= INFO
) {
   if ( fileLevel == 'OFF' ) {
      fileLevel <- 0
   }
   if ( consoleLevel == 'OFF' ) {
      consoleLevel <- 0
   }
   flog.logger( packageName() %p% ".file", fileLevel, appender=appender.file( file ))
   flog.logger( packageName() %p% ".console", consoleLevel, appender=appender.console() )
}
