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
    v <- unclass(dict)[[key]] %||% orElse
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
`$<-.dict` <- function(dict, name, value) {
  if (!is.character(name)) {
    stop('Only character keys allowed.')
  }
  dict <- unclass(dict)
  dict[[name]] <- value
  class(dict) <- c('dict', 'list')
  dict
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
`keys<-` <- function(object, value) {
  names(object) <- unique(value)
  object
}

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
`keys<-` <- `names<-`

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
defaults <- function(dict, defaults) {
  missing_keys <- setdiff(keys(defaults), keys(dict))
  dict[missing_keys] <- defaults[missing_keys]
  dict
}

#' @export
has <- function(dict, key) {
  key %in% keys(dict)
}

