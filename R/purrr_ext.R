
#' Map, filter, compact on keys and values of the dictionary
#'
#' \code{map_dict} calls a function on each (key, value) pair and builds a dictionary from the transformed input
#' (see \link[purrr]{map} in package \code{purrr}).
#' \code{keep_dict} and \code{discard_dict} keep and discard elements based on the return value of the
#' predicate function (see \link[purrr]{keep} and \link[purrr]{discard} in package \code{purrr}).
#' \code{compact_dict} returns a dictionary with all the \code{NULL} values removed.
#' @param x A dictionary
#' @param .f A function, formula or character vector.
#' @param .p A predicate function returning a logical value.
#' If a function, it is called with two arguments, the key and the value.
#' If a formula, .x corresponds to the key and .y to the value.
#' If a character vector, it will return the values corresponding to the keys in the vector.
#' @return For \code{map_dict}, a dictionary with the same keys as \code{dict}, and values
#' given by the return value of \code{.f}. For \code{keep_dict}, \code{discard_dict}, and
#' \code{compact_dict}, a dictionary containing the entries that passed the filter.
#' @export
map_dict <- function(x, .f, ...) {
  if (! is_dict(x))
    stop('Object of class dict expected.')
  make_dict(keys(x), purrr::map2(keys(x), values(x), .f, ...))
}

#' @export
#' @rdname map_dict
keep_dict <- function(x, .p, ...) {
  if (! is_dict(x))
    stop('Object of class dict expected.')
  keys <- purrr::map2_lgl(keys(x), values(x), .p, ...)
  x[keys(x)[keys]]
}

#' @export
#' @rdname map_dict
discard_dict <- function(x, .p, ...) {
  if (! is_dict(x))
    stop('Object of class dict expected.')
  keys <- purrr::map2_lgl(keys(x), values(x), .p, ...)
  x[keys(x)[!keys]]
}

#' @export
#' @rdname map_dict
compact_dict <- function(x) {
  if (! is_dict(x))
    stop('Object of class dict expected.')
  discard_dict(x, function(k, v) is.null(v))
}
