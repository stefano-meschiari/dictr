#' @export
`+` <- function(e1, e2) {
  if (is.character(e1) || is.character(e2)) {
    str_c(e1, e2)
  } else {
    .Primitive("+")(e1, e2)
  }
}

#' @export
`%%` <- function(e1, e2) {
  if (is.character(e1)) {
    do.call(sprintf, c(e1, as.list(e2)))
  } else {
    .Primitive("%%")(e1, e2)
  }
}

#' @export
dict <- function(...) {
  data <- list(...)
  as.dict(data)
}

#' @export
default_dict <- function(..., default=NULL) {
  dict <- dict(...)
  default(dict) <- default
  dict
}

#' @export
strict_dict <- function(...) {
  dict <- dict(...)
  attr(dict, 'strict') <- TRUE
  dict
}

#' @export
`default<-` <- function(dict, value) {
  attr(dict, 'default') <- value
  dict
}

#' @export
default <- function(dict) {
  attr(dict, 'default')
}

#' @export
make_dict <- function(keys, values, default=NULL, strict=FALSE) {
  dict <- as.dict(set_names(values, keys))
  attr(dict, 'default') <- default
  if (strict)
    attr(dict, 'strict') <- TRUE
  dict
}

length.dict <- function(dict) {
  length(keys(dict))
}

#' @export
is.dict <- function(dict) {
  inherits(dict, 'dict')
}

#' @export
as.dict <- function(obj) {
  if (inherits(obj, 'entries')) {
    names <- map(obj, 'key')
    values <- map(obj, 'value')
    obj <- set_names(values, names)
  } else if (inherits(obj, 'entry')) {
    obj <- set_names(obj$value, obj$key)
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
`$.dict` <- function(dict, key, orElse=attr(dict, 'default')) {
  if (is.character(key)) {
    v <- NextMethod() %||% orElse
    if (is.null(v) && ! is.null(attr(dict, 'strict'))) {
      stop('Attempted access of non-existing key', key)
    } else {
      v
    }
  } else {
    stop('Only character keys allowed.')
  }
}

#' @export
`[[.dict` <- `$.dict`

as.list.dict <- function(dict) {
  unclass(dict)
}

#' @export
`==.dict` <- function(dict, other) {
  if (! is.dict(other))
    return(FALSE)

  keys <- union(keys(dict), keys(other))
  set_names(map_lgl(keys, ~ identical(dict[[.x]], other[[.x]])), keys)
}

#' @export
`$<-.dict` <- function(dict, key, value) {
  if (!is.character(key)) {
    stop('Only character keys allowed.')
  }

  keys <- keys(dict)

  if (! is.null(value)) {
    return(NextMethod())
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
`[[<-.dict` <- `$<-.dict`

#' @export
`[.dict` <- function(dict, ..., omit) {
  names <- c(...)
  if (!is.character(names))
    stop('Only character keys allowed.')

  make_dict(names, map(names, ~ dict[[.x]]))
}

#' @export
`[<-.dict` <- function(dict, names, value) {
  if (!is.character(names))
    stop('Only character keys allowed.')
  if (length(unique(names)) > length(value))
    stop('Not enough values for the specified keys.')
  for (i in seq_along(names))
    dict[[names[i]]] <- value[[i]]
  dict
}

#' @export
omit <- function(dict, ...) {
  names <- unlist(list(...))
  if (!is.character(names))
    stop('Only character keys allowed')
  names <- setdiff(keys(dict), names)
  dict[names]
}

#' @export
extend <- function(dict, ...) {
  UseMethod('extend', dict)
}

extend.dict <- function(dict, ...) {
  dots <- list(...)
  if (length(dots) == 0)
    return(dict)
  names <- names(dots)
  for (i in seq_along(dots)) {
    if (is.list(dots[[i]])) {
      dots[[i]] <- as.dict(dots[[i]])
      dict[keys(dots[[i]])] <- values(dots[[i]])
    } else {
      if (is.null(names) || names[i] == '')
        stop('Name not specified for argument ', i)
      dict[names[i]] <- dots[[i]]
    }
  }
  dict
}

#' @export
keys <- function(object) {
  unique(names(object))
}

#' @export
`keys<-` <- `names<-`

#' @export
values <- function(dict) {
  unname(unclass(dict))
}

#' @export
`values<-` <- function(dict, value) {
  make_dict(keys(dict), value)
}

#' @export
entries <- function(dict) {
  structure(mapkv(dict, entry), class='entries')
}

#' @export
`names<-.dict` <- function(dict, value) {
  attr(dict, 'names') <- unique(value)
  dict
}

#' @export
entry <- function(key, value) {
  if (!is.character(key))
    stop('Key should be character')

  structure(list(key=key, value=value), class=c('entry', 'list'))
}

print_kv <- function(key, value, key_width=NULL) {
  screen_width <- getOption('width')
  tc <- textConnection('printentry', 'w')
  on.exit({ options(width=screen_width); close(tc) })

  key_width <- 2 + key_width %||% str_length(key)
  if (is.na(key))
    key <- '<NA>'
  key <- '$ ' + as.character(key)

  if (str_length(key) - 3 > key_width) {
    fmt <- '%' + (key_width-3) + 's...'
  } else {
    fmt <- '%' + (key_width) + 's'
  }
  cat(sprintf(fmt, key), ' : ', sep='')

  new_width <- screen_width - key_width - 6

  options(width=new_width)
  sink(tc)
  print(value)
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

  key_width <- min(30, max(map_int(entries, ~ str_length(.x$key)), na.rm=TRUE))
  for (entry in entries)
    print_kv(entry$key, entry$value, key_width=key_width)
}

#' @export
print.dict <- function(dict) {
  if (length(dict) == 0) {
    cat('(empty dictionary)\n')
    return()
  }

  key_width <- min(30, max(str_length(keys(dict)), na.rm=TRUE))
  for (key in keys(dict)) {
    print_kv(key, dict[[key]], key_width=key_width)
  }
}

#' @export
str.dict <- function(dict, ...) {
  cat('Dict with', length(dict), 'keys')
  if (!is.null(attr(dict, 'default')))
    cat(' and default value')
  cat(', backed by ')
  str(unclass(dict), ...)
}

#' @export
mapkv <- function(dict, .f, ...) {
  if (! is.dict(dict))
    stop('Object of class dict expected.')
  ret <- map2(keys(dict), as.list(dict), .f, ...)
  keys(ret) <- keys(dict)
  ret
}

#' @export
mapkv_dict <- function(dict, .f, ...) {
  make_dict(keys(dict), mapkv(dict, .f, ...))
}

#' @export
invert <- function(dict) {
  make_dict(as.character(values(dict)),
            keys(dict))
}

#' @export
detect_key <- function(dict, condition, ..., right=FALSE) {
  keys(dict)[detect(dict, condition, ..., right=FALSE)]
}

#' @export
defaults <- function(x, defaults) {
  missing_keys <- setdiff(names(defaults), names(x))
  x[missing_keys] <- defaults[missing_keys]
  x
}

#' @export
has <- function(dict, key) {
  key %in% keys(dict)
}

.defaults <- dict(
  integer = 0L,
  logical = FALSE,
  numeric = 0,
  list = list(),
  dict = dict(),
  character = '',
  complex = complex(1),
  any = NULL
)

type_check <- function(val, type, name, struct) {
  if (is_formula(type)) {
    struct <- unclass(struct)
    struct[[name]] <- val

    constraint_satisfied <- tryCatch(f_eval_rhs(type, struct), error=function(...) FALSE)

    if (! isTRUE(constraint_satisfied)) {
      stop('Field "', name, '" does not satisfy constraint: ', as.character(f_rhs(type))[-1])
    } else
      return(TRUE)
  } else {
    if (length(type == 1) && type == 'any')
      return(TRUE)
    else if (is.null(val))
      stop('Field "', name, '" needs to be specified; you passed NULL or nothing instead.')
    else if (type[1] == 'either')
      type <- type[-1]

    if (!inherits(val, type))
      stop('Attempted to set field "', name, '" with value of type ', str_c(class(val), collapse=', '),
           ', expected: ', str_c(type, collapse=', '))
    else
      return(TRUE)
  }
}


#' @export
struct <- function(.name, ...) {
  if (! is.character(.name)) {
    stop("Specify the name of the struct")
  }

  pars <- list(...)
  if (length(pars) == 0)
    stop('No fields specified.')

  names <- names(pars)
  if (is.null(names))
    names <- rep('', length(pars))

  template <- dict()
  types <- dict()

  for (i in seq_along(pars)) {
    if (names[i] == '' && !is_formula(pars[[i]]))
      stop("Arguments should either specify a default value (e.g. 'x = 0') or a type (e.g. 'x ~ integer')")

    if (names[i] == '') {
      # Argument is specified as name ~ class, e.g. x ~ integer
      f <- pars[[i]]
      type <- as.character(f_rhs(f))
      name <- as.character(f_lhs(f))

      names[i] <- name
      types[[names[i]]] <- type
      template[[names[i]]] <- NULL

    } else if (is_formula(pars[[i]])) {
      # Both class and default values are specified as name = default ~ class, e.g. x = 0 ~ integer
      f <- pars[[i]]
      types[[names[i]]] <- as.character(f_rhs(f))
      template[[names[i]]] <- f_eval_lhs(f)
    } else {
      # Only default is passed, e.g. x = 0
      types[[names[i]]] <- class(pars[[i]])
      template[[names[i]]] <- pars[[i]]
    }

    if (types[[names[i]]][1] == '{')
      types[[names[i]]] <- pars[[i]]
  }

  for (k in keys(template)) {
    if (!is.null(template[[k]]))
      type_check(template[[k]], types[[k]], k, template)
  }

  struct_new <- function_new(template, quote({
    # Creates a new struct.
    .struct <- template

    for (k in keys(template)) {
      .struct[[k]] <- get(k)
    }

    attr(.struct, 'types') <- types
    class(.struct) <- c(.name, 'struct', class(.struct))

    for (k in keys(.struct))
      type_check(.struct[[k]], types[[k]], k, .struct)
    .struct
  }))

  attr(struct_new, 'struct_name') <- .name
  attr(struct_new, 'types') <- types
  class(struct_new) <- c('struct_new', 'function')
  struct_new
}

#' @export
is.struct <- function(struct) {
  inherits(struct, 'struct') && !is.null(attr(struct, 'types'))
}

#' @export
print.struct <- function(struct) {
  class <- class(struct)
  types <- attr(struct, 'types')
  cat('Struct of class ', class[1], '\n')
  print.dict(struct)
}

#' @export
print.struct_new <- function(f) {
  describe(f)
}

#' @export
describe <- function(struct) {
  if (is.function(struct))
    class <- attr(struct, 'struct_name')
  else
    class <- class(struct)[1]

  types <- attr(struct, 'types')

  cat('Definition of struct of class:', class, '\n')
  cat('\nFields:\n')
  for (k in keys(types)) {
    type <- types[[k]]
    if (is_formula(types[[k]]))
      type <- as.character(f_rhs(type))[-1]

    cat('$', k, ' ~ ', type, '\n')
  }
}

#' @export
`$<-.struct` <- function(struct, key, value) {
  if (!is.character(key))
    stop('Key should be a character value.')
  if (!is.struct(struct))
    stop('Malformed struct.')
  if (! key %in% keys(struct))
    stop('Field "', key, '" does not exist in the definition of struct ', class(struct)[1])
  types <- attr(struct, 'types')
  type_check(value, types[[key]], key, struct)
  NextMethod()
}

`[[<-.struct` <- `$<-.struct`

#' @export
`[<-.struct` <- function(struct, keys, values) {
  if (!is.struct(struct))
    stop('Malformed struct.')

  if (length(setdiff(keys, keys(struct))) != 0)
    stop('Unknown fields specified: ', str_c(setdiff(keys, keys(struct)), sep=', '))

  class <- class(struct)
  class(struct) <- 'dict'
  struct[keys] <- values
  class(struct) <- class

  types <- attr(struct, 'types')
  for (k in keys(struct))
    type_check(struct[[k]], types[[k]], k, struct)

  struct
}
