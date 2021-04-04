
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
#' @param key index or indices specifying the value to access or replace
#' @param ... additional keys
#' @param value list or vector of value(s) to replace
#' @name dict_operators
NULL

#' @rdname dict_operators
#' @export
`[[.dict` <- function(dict, key) {
  if (is.character(key) && length(key) == 1) {
    v <- if (key != "") {
      NextMethod(exact=TRUE)
    } else {
      # special-case empty string, since list[[""]] will return NULL. Use
      # positional access instead.
      position <- which(names(dict) == "")
      if (length(position) > 0) {
        unclass(dict)[[position]]
      } else {
        NULL
      }
    }
    if (! is.null(v)) {
      v
    } else if (!is.null(attr(dict, 'strict'))) {
      stop('Attempted access of non-existing key', key)
    } else {
      attr(dict, 'default')
    }
  } else {
    if (! is.character(key)) {
      stop('Only character keys are allowed.')
    }
    if (length(key) != 1) {
      stop('Can only address the value for a single key with [[]]. Use [] to retrieve multiple values.')
    }
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
  if (!is.character(key) || is.null(key)) {
    stop('Only character keys allowed.')
  } else if (is.na(key)) {
    stop('NA keys are not allowed.')
  }
  if (key != "" && !is.null(value)) {
    dict <- unclass(dict)
    dict[[key]] <- value
    class(dict) <- c("dict", "list")
    return(dict)
  }

  keys <- keys(dict)
  cl <- class(dict)
  dict <- unclass(dict)

  if (is.null(value) && key %in% keys) {
    idx <- which(key == keys)
    kidx <- seq_along(keys)
    dict <- values(dict)

    dict <- c(dict[kidx < idx],
              list(NULL),
              dict[kidx > idx])
  } else if (! key %in% keys) {
    keys <- c(keys, key)
    dict <- c(dict, list(value))
  }
  names(dict) <- keys
  class(dict) <- cl
  dict
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
  if (!is.character(keys)) {
    stop('Only character keys allowed.')
  }
  if (length(unique(keys)) > length(value)) {
    stop('Not enough values for the specified keys.')
  }
  if (! is.null(names(value))) {
    for (key in keys) {
      dict[[key]] <- value[[key]]
    }
  } else {
    for (i in seq_along(keys))
      dict[[keys[i]]] <- value[[i]]
  }
  dict
}

#' Compare dictionary objects
#'
#' Returns a logical vector that maps each key to a logical value; the value is TRUE
#' if the two objects have same values for the key,
#' or FALSE if the values are not identical. If one of the two objects does not
#' contain the key, the vector will have a value of NA for that key.
#'
#' To test whether two dictionary objects have the same contents and return
#' a single \code{TRUE} or \code{FALSE}, use \link{equal}.
#'
#' @param dict dictionary
#' @param other another dictionary
#'
#' @export
`==.dict` <- function(dict, other) {
  if (! is_dict(dict)) {
    stop("Expected object of type dict on the right hand side")
  }
  keys_dict <- keys(dict)
  keys_other <- keys(other)

  keys <- union(keys_dict, keys_other)

  result <- purrr::map_lgl(keys, function(key) {
    if (! key %in% keys_dict || ! key %in% keys_other ) {
      NA
    } else {
      if (is.null(dict[[key]])) {
        is.null(other[[key]])
      } else if (is.null(other[[key]])) {
        is.null(dict[[key]])
      } else {
        tryCatch(all(dict[[key]] == other[[key]]), error=function(e) {
          stop("Error during comparison of values at key '", key, "': ", e$message)
        })
      }
    }
  })
  names(result) <- keys
  result
}


#' Compare dictionary objects
#'
#' Returns \code{TRUE} if the two dicts have the same keys and values,
#' \code{FALSE} otherwise.
#'
#' @param dict dictionary
#' @param other another dictionary
#'
#' @export
equal <- function(dict, other) {
  if (!is_dict(dict)) {
    stop("Expected object of type dict for argument 'dict'")
  }
  if (!is_dict(other)) {
    stop("Expected object of type dict for argument 'other'")
  }
  isTRUE(all(dict==other))
}
