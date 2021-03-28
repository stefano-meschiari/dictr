
#' Access or replace values of a dictionary
#'
#' Access or replace one or multiple values of a dictionary using keys.
#'
#' These operators work equivalently to the list operators \code{[}, \code{[[} and \code{$}.
#' These operators do not partially match keys. Additionally, assigning \code{NULL} to one
#' of the keys does not remove the entry from the dictionary; instead, it sets the value
#' corresponding to that key to \code{NULL} (use \link{omit} to remove entries).
#'
#' @param dict object from which to access value(s) or in which to replace value(s)
#' @param key, ... index or indices specifying the value to access or replace
#' @param value list or vector of value(s) to replace
#' @name dict_operators
NULL

#' @rdname dict_operators
#' @export
`[[.dict` <- function(dict, key) {
  if (is.character(key) && length(key) == 1) {
    v <- NextMethod(exact=TRUE)
    if (! is.null(v)) {
      v
    } else if (!is.null(attr(dict, 'strict'))) {
      stop('Attempted access of non-existing key', key)
    } else
      attr(dict, 'default')
  } else {
    stop('Only a single character key allowed.')
  }
}

#' @export
#' @rdname dict_operators
`$.dict` <- function(dict, key) {
  dict[[key]]
}

#' @export
#' @rdname dict_operators
`$<-.dict` <- function(dict, key, value) {
  if (!is.character(key))
    stop('Only character keys allowed.')

  if (! is.null(value)) {
    cl <- class(dict)
    dict <- unclass(dict)
    dict[[key]] <- value
    class(dict) <- cl
    dict
  } else {
    keys <- keys(dict)
    dict <- unclass(dict)
    if (! key %in% keys) {
      keys <- c(keys, key)
      dict <- c(dict, list(NULL))
    } else {
      idx <- which(key == keys)
      kidx <- seq_along(keys)
      dict <- values(dict)

      dict <- c(dict[kidx < idx],
                list(NULL),
                dict[kidx > idx])
    }
    names(dict) <- keys
    class(dict) <- c('dict', 'list')
    dict
  }
}

#' @export
#' @rdname dict_operators
`[[<-.dict` <- `$<-.dict`

#' @export
#' @rdname dict_operators
`[.dict` <- function(dict, ...) {
  keys <- c(...)
  if (!is.character(keys)) {
    stop('Only character keys allowed.')
  }
  make_dict(keys, purrr::map(keys, ~ dict[[.x]]))
}

#' @export
#' @rdname dict_operators
`[<-.dict` <- function(dict, ..., value) {
  keys <- c(...)
  if (!is.character(keys))
    stop('Only character keys allowed.')
  if (length(unique(keys)) > length(value))
    stop('Not enough values for the specified keys.')

  if (! is.null(keys(value))) {
    for (key in keys)
      dict[[key]] <- value[[key]]
  } else {
    for (i in seq_along(keys))
      dict[[keys[i]]] <- value[[i]]
  }
  dict
}


#' Checks whether two dictionary contain the same values
#'
#' Returns a logical vector that maps each key to a logical value; the value is TRUE
#' if the two dictionaries have identical values for the key (see \link[base]{identical}),
#' or FALSE if the values are not identical. If one of the two dictionaries does not
#' contain the key, the vector will have a value of NA for that key.
#'
#' @param dict dictionary
#' @param other another dictionary
#'
#' @export
`==.dict` <- function(dict, other) {
  if (! is_dict(other))
    stop("Expected object of type dict on the right hand side")

  keys_dict <- keys(dict)
  keys_other <- keys(other)

  keys <- union(keys_dict, keys_other)

  result <- purrr::map_lgl(keys, function(key) {
    if (! key %in% keys_dict || ! key %in% keys_other ) {
      NA
    } else {
      identical(dict[[key]], other[[key]])
    }
  })
  names(result) <- keys
  result
}

#' Checks if two dictionaries are the same
#'
#' Checks if two dictionaries are the same mapping.
#' Unlike \code{identical}, key ordering is ignored.
#'
#' @param x Two dictionaries
#' @param y
#'
#' @export
equal <- function(x, y) {
  if (!is_dict(x)) {
    stop("Expected object of type dict")
  }
  isTRUE(all(x == y))
}
