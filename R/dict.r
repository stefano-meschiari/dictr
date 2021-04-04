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

    assert_valid_keys(dots_names)
    structure(list(...), names=dots_names)
  }

  data <- key_args(...)

  as_dict(data)
}

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
#' # create an employee dict out of keys and values
#' employee <- make_dict(keys=c('name', 'age'), values=list('John', 60))
#'
#' # create a dict by applying a function to the keys
#' letter_to_LETTER <- make_dict(letters, toupper)
make_dict <- function(keys, values, default=NULL, strict=FALSE) {
  dict <- if (purrr::is_function(values) || purrr::is_formula(values)) {
    fn <- purrr::as_mapper(values)
    make_dict(keys, purrr::map(keys, fn))
  } else {
    as_dict(purrr::set_names(values, keys))
  }
  if (!is.null(default)) {
    attr(dict, 'default') <- default
  }
  if (strict) {
    attr(dict, 'strict') <- TRUE
  }
  dict
}

#' Tests whether the object is a dictionary.
#'
#' @param obj object to be tested
#'
#' @export
is_dict <- function(obj) {
  inherits(obj, 'dict') && is.list(obj)
}

is_dict_like <- function(obj) {
  names <- names(obj)

  is.list(obj) &&
    !is.null(names) &&
    !any(is.na(names) &&
    length(unique(names)) == length(names))
}

is_entry <- function(obj) {
  all(c("key", "value") %in% names(obj)) &&
    is.character(obj[["key"]])
}

#' Coerces an object to a dictionary
#'
#' Coerces a named list, a vector, or a list of \link{entry} tuple, to a dictionary.
#'
#' @param obj a named list or vector, or a list of \code{entry} tuples.
#' @return a dictionary containing the same keys and values as the input object.
#'
#' @export
as_dict <- function(obj) {
  if (is_dict(obj)) {
    return(obj)
  }
  if (length(obj) == 0) {
    return(structure(list(), class=c('dict', 'list')))
  }

  names <- names(obj)
  obj <- as.list(obj)

  # obj is a list of entry() objects
  is_list_of_entries <- is.list(obj) &&
    all(purrr::map_lgl(obj, is_entry))

  if (is_list_of_entries) {
    names <- purrr::map_chr(obj, 'key')
    values <- purrr::map(obj, 'value')
    obj <- purrr::set_names(values, names)
  }

  # check that keys exist and are unique
  assert_valid_keys(names)

  structure(obj, class=c('dict', 'list'))
}

#' @export
#' @rdname as_dict
as.dict <- as_dict

#' Coerces the dictionary to a named list
#'
#' @param x dictionary
#' @param ... ignored
#' @return a named list with the same keys and values
#' @export
as.list.dict <- function(x, ...) {
  unclass(x)
}

#' @export
#' @rdname extend
defaults <- function(x, defaults) {
  UseMethod('defaults', x)
}

defaults.list <- function(x, defaults) {
  missing_keys <- setdiff(names(defaults), names(x))
  x[missing_keys] <- defaults[missing_keys]
  x
}

#' Returns or assigns the keys of the provided dictionary, that ensures keys
#' are always unique.
#'
#' @param dict a dictionary
#' @param value new keys to assign
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
  `names<-.dict`(dict, value)
}

#' @export
`names<-.dict` <- function(x, value) {
  assert_keys_and_values_same_length(value, x)
  assert_valid_keys(value)
  attr(x, 'names') <- value
  x
}

#' Returns or assigns the values of the provided dictionary.
#'
#' @param dict a dictionary
#' @param value new values
#' @return a list containing the values of the dictionary, or the updated
#'   dictionary
#' @export
#' @examples
#' map <- dict(a=1, b=2)
#'
#' values(map) <- list("A", "B") # now map == dict(a="A", b="B")
values <- function(dict) {
  UseMethod('values', dict)
}

#' @export
values.list <- function(dict) {
  unname(unclass(dict))
}

#' @rdname values
#' @export
`values<-` <- function(dict, values) {
  make_dict(keys(dict), values)
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
#' library(purrr)
#' solar_system <- dict(Mercury = 0.387, Venus = 0.723, Earth = 1, Mars = 1.524,
#'   Jupiter=5.20, Saturn=9.58, Uranus=19.2, Neptune=30.1)
#' for (e in entries(solar_system))
#'    cat('The distance between planet', e$key, ' and the Sun is', e$value, ' AU.\n')
#'
#' # filter to only planets
#' inner_solar_system <- entries(solar_system) %>%
#'              keep(function(e) e$value <= solar_system$Mars) %>%
#'              as_dict()
#'
#' # alternative using keep_dict
#' outer_solar_system <- keep_dict(solar_system,
#'                                 function(key, value) value > solar_system$Mars)
#'
#' # get all values of the dictionary
#' semi_major_axes <- purrr::map_dbl(entries(solar_system), "value")
entries <- function(dict) {
  structure(purrr::map2(keys(dict), values(dict), entry),
            class=c('entries', 'list'))
}


#' Returns a list with elements \code{key} and \code{value}.
#' @param key the key
#' @param value the value
#' @export
entry <- function(key, value) {
  structure(list(key=key, value=value), class=c('entry', 'list'))
}

#' @importFrom purrr %||%
purrr::`%||%`

print_kv <- function(key, value, key_width=NULL, ...) {
  screen_width <- getOption('width')
  tc <- textConnection('printentry', 'w')
  on.exit({ options(width=screen_width); close(tc) })

  key_width <- 3 + key_width %||% stringr::str_length(key)

  key <- stringr::str_c('$ ', as.character(key))
  padded_key <- stringr::str_pad(stringr::str_trunc(key, key_width), key_width)
  cat(padded_key, ' : ', sep='')

  new_width <- screen_width - key_width - 6

  options(width=new_width)
  sink(tc)
  tryCatch({print(value, ...)}, finally={sink(NULL)})

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
#' @param ... other parameters forwarded to print()
#'
#' @export
print.dict <- function(x, ...) {
  if (length(x) == 0) {
    cat('(empty dictionary)\n')
    return()
  }

  keys_to_print <- map_dict(x, function(k, v) {
    if (k == "") {
      '""'
    } else {
      k
    }
  })

  n <- 1
  key_width <- min(30, max(stringr::str_length(values(keys_to_print)), na.rm=TRUE))

  for (key in keys(x)) {
    print_kv(keys_to_print[[key]], x[[key]], key_width=key_width, ...)
    n <- n+1
    if (n > getOption('max.print')) {
      cat(sprintf('[ reached getOption("max.print") -- omitted %d entries. ]', length(x)-n))
      break
    }
  }
  invisible(x)
}

#' @export
#' @importFrom utils str
str.dict <- function(object, ...) {
  cat('Dict with', length(object), 'keys')
  if (!is.null(attr(object, 'default')))
    cat(' and default value')
  cat('\n')
  str(unclass(object), ...)
}

