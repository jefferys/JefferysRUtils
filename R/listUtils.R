# listUtils.R - Utility functions

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
#' @param keepOrder How to order the list when duppliates occur: by deafult
#'  this is FALSE, with duplicates dropped from \code{x} when they have the
#'   same name as an element in \code{y}, before appending \code{y}. If TRUE,
#'   preserves the order of element names as much as possible, by replacing the
#'   value of duplicated element names with those from \code{y}, then appending
#'   \code{y} with duplicated elements dropped.
#' @param ... Required for generic method. Not used.
#'
#' @return A list consisting of all elements from \code{x} not also in \code{y},
#'   and then all elements in \code{y}, or
#' @export
merge.list <- function(x, y, keepOrder= FALSE, ...) {
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

