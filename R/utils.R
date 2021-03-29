
#' Inverts a dictionary
#'
#' Returns a dictionary where keys and values are swapped. Values are coerced to character
#' keys, if needed.
#'
#' @param dict a dictionary
#' @return a new dictionary with keys and values swapped
#'
#' @export
invert <- function(dict) {
  UseMethod('invert', dict)
}

#' @export
invert.dict <- function(dict) {
  make_dict(as.character(values(dict)),
            keys(dict))
}

#' Finds the key of the first match
#'
#' See \link[purrr]{detect}. This function calls the predicate \code{.p} on the
#' values of \code{dict}, and returns the first key for which the predicate is \code{TRUE}.
#'
#' @param dict a dictionary
#' @param .p a single predicate function (see \link[purrr]{detect}), which is passed the values of dict
#' @param ... additional parameters passed to \code{.p}
#' @param .right whether to start the search from the beginning or end of the dictionary
#' @return the first match for which the predicate is TRUE
#' @export
detect_key <- function(dict, .p, ..., .right=FALSE) {
  keys(dict)[purrr::detect_index(values(dict), .p, ..., .right=.right)]
}

#' Check whether a dictionary contains the specified key(s)
#'
#' @param dict a dictionary
#' @param keys a character vector of keys
#' @export
has <- function(dict, keys) {
  UseMethod('has', dict)
}

#' @export
has.list <- function(dict, keys) {
  keys %in% keys(dict)
}

#' Removes key(s) from a dictionary or named list
#'
#' Returns a new dictionary or named list with the specified keys omitted.
#'
#' @param dict dictionary or named list
#' @param ... character vector(s) of keys to remove
#' @return a new object without the entries corresponding to the specified keys
#' @export
omit <- function(dict, ...) {
  UseMethod('omit', dict)
}

#' @export
omit.list <- function(dict, ...) {
  keys <- unlist(c(...))

  if (!is.character(keys))
    stop('Only character keys allowed')

  keys <- c(setdiff(keys(dict), keys))
  dict[keys]
}

#' @export
omit.dict <- omit.list

#' Merge two dictionaries or named lists.
#'
#' @details
#' \code{c} or \code{extend} overrides the entries in the first dictionary with
#' entries from the following objects, left to right (each object overrides
#' entries from the previous argument).
#'
#' \code{defaults} augments the entries in the first dictionary with entries from the second dictionary
#' that were not present in the first.
#'
#' @param x original object; a dictionary or named list
#' @param ... dictionaries or named lists that override \code{x}. Named parameters
#'   can also be used.
#' @param defaults dictionary or named list providing default values for \code{x}
#'
#' @export
#' @examples
#' x <- dict(a=1, b=2, c=3)
#' y <- dict(a="a", b="b")
#'
#' c(x, y) # returns dict(a="a", b="b", c=3)
#' c(x, c=9, d=16) # returns dict(a=1, b=2, c=9, d=16)
#' defaults(x, dict(a=10, d=10)) # returns dict(a=1, b=2, c=3, d=10)
extend <- function(x, ...) {
  UseMethod('extend', x)
}

#' @export
extend.list <- function(x, ...) {
  dots <- list(...)
  if (length(dots) == 0)
    return(x)
  names <- names(dots)
  for (i in seq_along(dots)) {
    if (is.list(dots[[i]])) {
      dots[[i]] <- as_dict(dots[[i]])
      x[keys(dots[[i]])] <- values(dots[[i]])
    } else {
      if (is.null(names) || names[i] == '')
        stop('Name not specified for argument ', i)
      x[[names[i]]] <- dots[[i]]
    }
  }
  x
}

#' @export
extend.dict <- extend.list

#' @export
c.dict <- function(...) {
  extend.dict(...)
}

#' @rdname extend
#' @export
defaults <- function(x, defaults) {
  c(defaults, x)
}

#' Renames existing keys of a dictionary
#'
#' @param x A dictionary
#' @param ... Keys to rename, of the form \code{new_name = old_name}
#'
#' @return Dictionary with renamed keys
#' @export
rename_keys <- function(x, ...) {
  new_keys_dict <- invert(dict(...))

  existing_keys <- keys(x)
  new_keys <- as.character(values(new_keys_dict))
  old_keys <- keys(new_keys_dict)

  if (any(! old_keys %in% existing_keys)) {
    stop("One or more keys do not exist in the dictionary")
  }

  keys(x) <- purrr::map_chr(existing_keys,
                            ~ get_or_else(new_keys_dict, .x, .x))

  x
}

#' Returns value or default if key does not exist
#'
#' Returns the value associated with \code{key}, or the
#' value \code{orElse} if the key does not exist
#'
#' @param x A dictionary
#' @param key Key to retrieve
#' @param orElse Alternative value
#'
#' @return Value in dictionary, or alternative value if key does not exist
#' @export
get_or_else <- function(x, key, orElse) {
  if (key %in% keys(x)) {
    x[[key]]
  } else {
    orElse
  }
}
