#' Transforms any collection into an immutable collection.
#'
#' Returns a copy of the collection that cannot be modified using the bracket, double bracket
#' or dollar operators.
#'
#' \code{mutable} will make an immutable collection mutable again.
#'
#' @param coll collection to be made immutable
#' @return a copy of the collection, marked as immutable
#' @export
#' @examples
#' a <- list(1, 2, 3)
#' a[[1]] <- 2  # ok
#'
#' a <- immutable(a)
#' # a[[1]] <- 2  will throw an error
immutable <- function(coll) {
  UseMethod('immutable')
}

#' @export
immutable.default <- function(coll) {
  class(coll) <- c('immutable', class(coll))
  coll
}

#' @export
#' @rdname immutable
mutable <- function(coll) {
  UseMethod('mutable')
}

#' @export
mutable.default <- function(coll) {
  cl <- class(coll)
  class(coll) <- cl[cl != "immutable"]
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
`[<-.immutable` <- function(x, key, value) {
  stop('Attempting to mutate an immutable collection.')
}

#' @export
`[[<-.immutable` <- `[<-.immutable`

#' @export
`$<-.immutable` <- `[<-.immutable`

#' @export
print.immutable <- function(x, ...) {
  cat('(immutable collection)\n')
  cl <- class(x)
  class(x) <- cl[cl != "immutable"]
  NextMethod(x)
}
