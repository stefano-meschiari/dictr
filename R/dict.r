#' Creates a new dictionary
#'
#' Creates a new dictionary based on the specified key-value pairs.
#'
#' These functions are used to build a new dictionary object filled with the key = value pairs
#' passed as arguments. Typical dictionary objects are created using \code{dict}. If a key is not
#' specified, an implicit key is assumed using the name of the argument.
#'
#' Dictionary objects are similar to named lists, but have a few advantages over them.
#'
#' Firstly, all values contained in dictionary objects are always associated with a unique character key.
#' This means that values can only be accessed using their respective keys, and not using
#' integer indices, which is more consistent with the intended usage. Empty or
#' non-unique keys are strictly not allowed.
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
#' expect lists. They maintain keys in insertion order.
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
#' @param .default a default value to be returned when a non-existing key is accessed.
#' @return a new dictionary object.
#' @export
#'
#' @examples
#' person <- dict(name = "Joan", last_name = "Smith", age = 30)
#'
#' color <- 'blue'
#' pattern <- 'vertical'
#' fill <- dict(color, pattern) # equivalent to dict(color="blue", pattern="vertical")
#'
#' salaries <- default_dict(employee_A = 100, employee_B = 50,
#'                          employee_C = 75, .default = 60)
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

    assert_unique_keys(dots_names)
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
#' @param values a vector or list of values to be associated to the corresponding keys.
#'   Alternatively, a purrr-style mapper function can be passed, which will be
#'   called with each key as a parameter.
#' @param default a default value for the dictionary (see \link{default_dict})
#' @param strict whether the dictionary should be strict and raise an exception when a non-existing key is accessed (see \link{strict_dict})
#'
#' @return a new dictionary
#' @export
#' @examples
#' employee <- make_dict(c('name', 'age'), list('John', 60))
make_dict <- function(keys, values, default=NULL, strict=FALSE) {
  dict <- if (purrr::is_function(values) || purrr::is_formula(values)) {
    fn <- purrr::as_function(values)
    as_dict(keys, purrr::map(keys, fn))
  } else {
    as_dict(purrr::set_names(values, keys))
  }

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
#' Coerces a named list, a vector, or a list of \link{entries} to a dictionary.
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
    stop("Cannot coerce this object to a dictionary; not enough unique keys")
  }

  structure(obj[names(obj)], class=c('dict', 'list'))
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
  UseMethod('keys', dict)
}

#' @export
keys.list <- function(dict) {
  names(dict)
}

keys.dict <- keys.list

#' @rdname keys
#' @export
`keys<-` <- function(dict, value) {
  UseMethod('keys<-', dict)
}

#' @export
`keys<-.list` <- function(dict, value) {
  assert_unique_keys(value)
  names(dict) <- value
  dict
}

#' @export
`keys<-.dict` <- `keys<-.list`

#' Returns or assigns the values of the provided dictionary.
#'
#' @param dict a dictionary
#' @return a list containing the values of the dictionary
#' @export
values <- function(dict) {
  UseMethod('values', dict)
}

#' @export
values.list <- function(dict) {
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
#' the \link{entry} function. A list of entries can be collected back into a dictionary using the
#' the \link{as_dict} function. This function can be useful to loop over a
#' dictionary using \code{for} or one of the \code{purrr::map} functions.
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
#'              as_dict()
#'
#' semi_major_axes <- purrr::map_dbl(entries(solar_system), "value")
entries <- function(dict) {
  structure(purrr::map2(keys(dict), values(dict), entry),
            class=c('entries', 'list'))
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
  structure(list(key=key, value=value), class=c('entry', 'list'))
}

print_kv <- function(key, value, key_width=NULL, digits=digits) {
  screen_width <- getOption('width')
  tc <- textConnection('printentry', 'w')
  on.exit({ options(width=screen_width); close(tc) })

  key_width <- 2 + key_width %||% stringr::str_length(key)
  if (is.na(key)) {
    key <- '<NA>'
  }

  key <- stringr::str_c('$ ', as.character(key))
  cat(stringr::str_trunc(key, key_width), ' : ', sep='')

  new_width <- screen_width - key_width - 6

  options(width=new_width)
  sink(tc)
  tryCatch({print(value, digits=digits)}, finally={sink(NULL)})

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

