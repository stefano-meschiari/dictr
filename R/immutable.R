#' Transforms any collection into an immutable collection.
#'
#' Returns a copy of the collection that cannot be modified using the bracket, double bracket
#' or dollar operators.
#'
#' @param coll collection to be made immutable
#' @return a copy of the collection, marked as immutable
#' @export
immutable <- function(coll) {
  UseMethod('immutable')
}

#' @export
immutable.default <- function(coll) {
  class(coll) <- c('immutable', class(coll))
  coll
}

#' Checks whether a collection is immutable
#' @param coll collection
#' @return TRUE if immutable, FALSE otherwise
#' @export
is_immutable <- function(coll) {
  UseMethod('is_immutable', coll)
}

#' @export
is_immutable.default <- function(coll) {
  inherits(coll, 'immutable') && class(coll)[1] == 'immutable'
}

#' @export
`[<-.immutable` <- function(...) {
  stop('Attempting to mutate an immutable collection.')
}

#' @export
`[[<-.immutable` <- `[<-.immutable`

#' @export
`$<-.immutable` <- `[<-.immutable`

#' @export
print.immutable <- function(x, ...) {
  cat('(immutable collection)\n')
  NextMethod(x)
}
