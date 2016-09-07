#' Creates a new dictionary
#'
#' Creates a new dictionary based on the specified key/value pairs.
#'
#' @details
#' These functions are used to build a new dictionary object filled with the key = value pairs
#' passed as arguments. Typical dictionary objects are created using \code{dict}. If a key is not
#' specified, an implicit key is assumed.
#'
#' Dictionary objects are equivalent to named lists, but have a few advantages over them.
#'
#' Firstly, all values contained in dictionary objects are always associated with a unique character key.
#' This means that values can only be accessed using their respective keys, and not using
#' integer indices, which is more consistent with the intended usage. Empty or repeated keys
#' are not allowed.
#'
#' Secondly, keys are never partially matched; for instance, \code{my_dict$k} will not match
#' \code{my_dict$key} but will instead return \code{NULL}.
#'
#' Dictionary object can store \code{NULL}s as values; assigning \code{NULL} to a key will
#' not delete that key from the dictionary, but set the value associated with that key to \code{NULL}.
#' To remove a key, use the \link{omit} function.
#'
#' Finally, printing of dicts is more compact that printing named lists.
#'
#' Dictionaries are implemented using named lists, so they can be passed to functions that
#' expect lists.
#'
#' \code{default_dict} creates a dictionary with default values. When the user accesses a
#' non-existing key, the default value will be returned instead of \code{NULL}.
#'
#' \code{strict_dict} creates a dictionary such that when the user accesses a non-existing key,
#' an exception will be raised instead of returning \code{NULL}.
#'
#' \code{immutable_dict} creates a dictionary that cannot be modified (see \link{immutable}).
#'
#' @param ... key and value pairs. All arguments must be named.
#' @param default a default value to be returned when a non-existing key is accessed.
#' @return a new dictionary object.
#' @export
#'
#' @examples
#' person <- dict(name = "Joan", last_name = "Smith", age = 30)
#'
#' color <- 'blue'
#' pattern <- 'vertical'
#' fill <- dict(color, pattern)
#'
#' salaries <- default_dict(employee_A = 100, employee_B = 50,
#'                          employee_C = 75, default = 60)
#'
#' enumeration <- strict_dict(YES = 1, NO = 0)
dict <- function(...) {

  key_args <- function(...) {
    dots <- eval(substitute(alist(...)))
    if (is.null(names(dots)))
      names(dots) <- rep('', length(dots))
    implicit_names <- as.character(dots[names(dots) == ""])
    if (!identical(make.names(implicit_names), implicit_names)) {
      non_syntactic <- implicit_names[make.names(implicit_names) != implicit_names]
      stop('Implicit keys are not valid for arguments: ', paste(non_syntactic))
    }
    dots_names <- ifelse(names(dots) == "", as.character(dots), names(dots))

    if (length(unique(dots_names)) != length(dots_names)) {
      repeated <- table(dots_names)[table(dots_names) > 1]
      stop('The following keys are specified more than once: ', paste(names(repeated), collapse=' '))
    }

    structure(list(...), names=dots_names)
  }


  data <- key_args(...)

  as_dict(data)
}

`%||%` <- purrr::`%||%`

#' @rdname dict
#' @export
default_dict <- function(..., .default=NULL) {
  dict <- dict(...)
  default(dict) <- .default
  dict
}

#' @rdname dict
#' @export
strict_dict <- function(...) {
  dict <- dict(...)
  attr(dict, 'strict') <- TRUE
  dict
}

#' @rdname dict
#' @export
immutable_dict <- function(...) {
  immutable(dict(...))
}

#' Gets and sets the default value of a dictionary.
#'
#' Functions to get and set the default value of a dictionary. The default value is returned
#' when a non-existing key is accessed.
#'
#' @param dict an existing dictionary object
#' @param value the new default value
#'
#' @return the modified dictionary
#' @export
default <- function(dict) {
  attr(dict, 'default')
}

#' @export
#' @rdname default
`default<-` <- function(dict, value) {
  attr(dict, 'default') <- value
  dict
}

#' Create a new dictionary from a list or vector of keys and values
#'
#' Create a new dictionary from separate keys and values.
#'
#' @param keys a vector of character keys
#' @param values a vector or list of values to be associated to the corresponding keys
#' @param default a default value for the dictionary (see \link{default_dict})
#' @param strict whether the dictionary should be strict and raise an exception when a non-existing key is accessed (see \link{strict_dict})
#'
#' @return a new dictionary
#' @export
#' @examples
#' employee <- make_dict(c('name', 'age'), list('John', 60))
make_dict <- function(keys, values, default=NULL, strict=FALSE) {
  dict <- as_dict(purrr::set_names(values, keys))
  attr(dict, 'default') <- default
  if (strict)
    attr(dict, 'strict') <- TRUE
  dict
}

#' Get the length of the dictionary
#'
#' Returns the number of unique keys in the dictionary.
#'
#' @param dict a dictionary object
#'
#' @return the number of unique keys stored in the dictionary.
#' @export
length.dict <- function(dict) {
  length(keys(dict))
}

#' Tests whether the object is a dictionary.
#'
#' @param obj object to be tested
#'
#' @export
is_dict <- function(obj) {
  inherits(obj, 'dict') && is.list(obj)
}

#' Coerces an object to a dictionary
#'
#' Coerces a named list, a vector, or a list of \link{entries} to a dictionary. \code{collect}
#' and \code{as_dict} are synonymous.
#'
#' @param obj a named list or vector.
#' @return a dictionary containing the same keys and values as the input object.
#'
#' @export
as_dict <- function(obj) {
  if (is_dict(obj))
    return(obj)

  if (inherits(obj, 'entries')) {
    names <- purrr::map_chr(obj, 'key')
    values <- purrr::map(obj, 'value')
    obj <- purrr::set_names(values, names)
  } else if (inherits(obj, 'entry')) {
    obj <- purrr::set_names(obj$value, obj$key)
  }

  obj <- as.list(obj)
  if (length(obj) > 0 &&
        is.null(names(obj)) ||
        length(unique(names(obj))) != length(obj)) {
    stop("Cannot coerce this object to a dictionary; not enough keys")
  }

  structure(obj, class=c('dict', 'list'))
}

#' @export
#' @rdname as_dict
collect <- as_dict

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
    v <- unclass(dict)[[key]] %||% attr(dict, 'default')
    if (is.null(v) && ! is.null(attr(dict, 'strict'))) {
      stop('Attempted access of non-existing key', key)
    } else {
      v
    }
  } else {
    stop('Only a single character key allowed.')
  }
}

#' @export
#' @rdname dict_operators
`$.dict` <- `[[.dict`

#' @export
#' @rdname dict_operators
`$<-.dict` <- function(dict, key, value) {
  if (!is.character(key)) {
    stop('Only character keys allowed.')
  }

  keys <- keys(dict)

  if (! is.null(value)) {
    dict <- unclass(dict)
    dict[[key]] <- value
    class(dict) <- c('dict', 'list')
    return(dict)
  } else {
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
  if (!is.character(keys))
    stop('Only character keys allowed.')

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


#' Coerces the dictionary to a named list
#'
#' @param dict dictionary
#'
#' @return a named list with the same keys and values
#' @export
as.list.dict <- function(dict) {
  unclass(dict)
}

#' Checks whether two dictionary contain the same values
#'
#' Returns a vector that maps each key to a logical value; the value is TRUE
#' if the two dictionaries have identical values for the key (see \link[base]{identical}),
#' or FALSE if the values are not identical or one of the two dictionaries does not
#' contain the key.
#'
#' @param dict dictionary
#' @param other another dictionary
#'
#' @export
`==.dict` <- function(dict, other) {
  if (! is_dict(other))
    return(FALSE)

  keys <- union(keys(dict), keys(other))
  purrr::set_names(purrr::map_lgl(keys, ~ identical(dict[[.x]], other[[.x]])), keys)
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
  keys <- c(...)
  if (!is.character(keys))
    stop('Only character keys allowed')

  keys <- setdiff(keys(dict), keys)
  dict[keys]
}

#' @export
omit.dict <- omit.list

#' Merge two dictionaries or named lists.
#'
#' @details
#' \code{extend} overrides the entries in the first dictionary with entries from the following objects,
#' left to right (each object overrides entries from the previous argument).
#'
#' \code{defaults} augments the entries in the first dictionary with entries from the second dictionary
#' that were not present in the first.
#'
#' @param x original object; a dictionary or named list
#' @param ... dictionaries or named lists that override \code{x}
#' @param defaults dictionary or named list providing default values for \code{x}
#'
#' @export
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
      x[names[i]] <- dots[[i]]
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

#' @export
#' @rdname extend
defaults <- function(x, defaults) {
  UseMethod('defaults', x)
}

#' @export
defaults.list <- function(x, defaults) {
  missing_keys <- setdiff(names(defaults), names(x))
  x[missing_keys] <- defaults[missing_keys]
  x
}

#' Returns or assigns the keys of the provided dictionary.
#'
#' @param dict a dictionary
#' @return a character vector containing the keys of the dictionary
#' @export
keys <- function(dict) {
  unique(names(dict))
}

#' @rdname keys
#' @export
`keys<-` <- `names<-`

#' Returns or assigns the values of the provided dictionary.
#'
#' @param dict a dictionary
#' @return a list containing the values of the dictionary
#' @export
values <- function(dict) {
  unname(unclass(dict))
}

#' @rdname values
#' @export
`values<-` <- function(dict, value) {
  make_dict(keys(dict), value)
}

#' Converts a dictionary to a list of key/value pairs
#'
#' Converts a dictionary into a list of entries containing key/value pairs, of the form
#' \code{list(entry(key = key1, value = value1), entry(key = key2, value = value2), ...)}.
#' Each element of the list is a list containing a key element and a value element, created using
#' the \link{entry} function. A list of entries can be collected back into a dictionary using
#' the \link{collect} function.
#'
#' @param dict a dictionary
#' @return a list containing lists with two items, \code{key} and \code{value}, for each entry in the
#' dictionary.
#' @export
#' @examples
#' solar_system <- dict(Mercury = 0.387, Venus = 0.723, Earth = 1, Mars = 1.524)
#' for (e in entries(solar_system))
#'    cat('The distance between planet', e$key, ' and the Sun is', e$value, ' AU.\n')
#'
#' inner_solar_system <- entries(solar_system) %>%
#'              keep(function(e) e$value <= 1) %>%
#'              collect()
entries <- function(dict) {
  structure(map_dict(dict, entry), class=c('entries', 'list'))
}

#' @export
`names<-.dict` <- function(dict, value) {
  attr(dict, 'names') <- unique(value)
  dict
}

#' Returns a list with elements \code{key} and \code{value}.
#' @param key the key
#' @param value the value
#' @export
entry <- function(key, value) {
  if (!is.character(key))
    stop('Key should be character')

  structure(list(key=key, value=value), class=c('entry', 'list'))
}

print_kv <- function(key, value, key_width=NULL, digits=digits) {
  screen_width <- getOption('width')
  tc <- textConnection('printentry', 'w')
  on.exit({ options(width=screen_width); close(tc) })

  key_width <- 2 + key_width %||% stringr::str_length(key)
  if (is.na(key))
    key <- '<NA>'

  key <- stringr::str_c('$ ', as.character(key))

  if (stringr::str_length(key) - 3 > key_width) {
    fmt <- stringr::str_c('%', (key_width-3), 's...')
  } else {
    fmt <- stringr::str_c('%', (key_width), 's')
  }
  cat(sprintf(fmt, key), ' : ', sep='')

  new_width <- screen_width - key_width - 6

  options(width=new_width)
  sink(tc)
  print(value, digits=digits)
  sink(NULL)

  val <- textConnectionValue(tc)
  if (length(val) > 0) {
    cat(val[1], '\n', sep='')
    for (l in val[-1]) {
      cat(rep(' ', key_width + 3), l, '\n', sep='')
    }
  } else {
    cat('\n')
  }
}

#' @export
print.entry <- function(entry) {
  print(unclass(entry))
}

#' @export
print.entries <- function(entries) {
  if (length(entries) == 0)
    return()

  key_width <- min(30, max(purrr::map_int(entries, ~ stringr::str_length(.x$key)), na.rm=TRUE))
  for (entry in entries)
    print_kv(entry$key, entry$value, key_width=key_width)
}

#' Prints the contents of a dictionary
#'
#' @param x dictionary
#' @param digits minimal number of significant digits
#'
#' @export
print.dict <- function(x, digits=NULL) {
  if (length(x) == 0) {
    cat('(empty dictionary)\n')
    return()
  }

  n <- 1
  key_width <- min(30, max(stringr::str_length(keys(x)), na.rm=TRUE))
  for (key in keys(x)) {
    print_kv(key, x[[key]], key_width=key_width, digits=digits)
    n <- n+1
    if (n > getOption('max.print')) {
      cat(sprintf('[ reached getOption("max.print") -- omitted %d entries. ]', length(x)-n))
      break
    }
  }
  invisible(x)
}

#' Display structure of dictionary
#'
#' See \link[utils]{str}.
#' @param dict a dictionary
#' @param ... additional parameters passed to \link[utils]{str}
#'
#' @export
str.dict <- function(dict, ...) {
  cat('Dict with', length(dict), 'keys')
  if (!is.null(attr(dict, 'default')))
    cat(' and default value')
  cat(', backed by ')
  str(unclass(dict), ...)
}

#' Map, filter, collect on keys and values of the dictionary
#'
#' \code{map_dict} calls a function on each (key, value) pair and builds a dictionary from the transformed input
#' (see \link[purrr]{map} in package \code{purrr}).
#' \code{keep_dict} and \code{discard_dict} keep and discard elements based on the return value of the
#' predicate function (see \link[purrr]{keep} and \link[purrr]{discard} in package \code{purrr}).
#' \code{compact_dict} returns a dictionary with all the \code{NULL} values removed.
#' @param dict A dictionary
#' @param .f A function, formula or character vector.
#' @param .p A predicate function returning a logical value.
#' If a function, it is called with two arguments, the key and the value.
#' If a formula, .x corresponds to the key and .y to the value.
#' If a character vector, it will return the values corresponding to the keys in the vector.
#' @return For \code{map_dict}, a dictionary with the same keys as \code{dict}, and values
#' given by the return value of \code{.f}. For \code{keep_dict}, \code{discard_dict}, and
#' \code{compact_dict}, a dictionary containing the entries that passed the filter.
#' @export
map_dict <- function(dict, .f, ...) {
  if (! is_dict(dict))
    stop('Object of class dict expected.')
  make_dict(keys(dict), purrr::map2(keys(dict), values(dict), .f, ...))
}

#' @export
#' @rdname map_dict
keep_dict <- function(dict, .p, ...) {
  keys <- purrr::map2_lgl(keys(dict), values(dict), .p, ...)
  dict[keys(dict)[keys]]
}

#' @export
#' @rdname map_dict
discard_dict <- function(dict, .p, ...) {
  keys <- purrr::map2_lgl(keys(dict), values(dict), .p, ...)
  dict[keys(dict)[!keys]]
}

#' @export
#' @rdname map_dict
compact_dict <- function(dict) {
  discard_dict(dict, function(k, v) is.null(v))
}


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
  keys(dict)[purrr::detect(values(dict), .p, ..., .right=.right)]
}

#' Check whether a dictionary contains the specified key(s)
#'
#' Equivalent to \code{key %in% keys(dict)}.
#' @param dict a dictionary
#' @param keys a character vector of keys
#' @export
has <- function(dict, keys) {
  keys %in% keys(dict)
}

#' Transforms any collection into an immutable collection.
#'
#' Returns a copy of the collection that cannot be modified using the bracket, double bracket
#' or dollar operators.
#'
#' @param coll collection to be made immutable
#' @return a copy of the collection, marked as immutable
#' @export
immutable <- function(coll) {
  class(coll) <- c('immutable', class(coll))
  coll
}

#' Checks whether a collection is immutable
#' @param coll collection
#' @return TRUE if immutable, FALSE otherwise
#' @export
is_immutable <- function(coll) {
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
  class(x) <- setdiff(class(x), 'immutable')
  NextMethod(x)
}
