library(inline)
library(Rcpp)
#' Computes the hash code of an arbitrary object.
#'
#' Computes the hash code of an object. The default implementation serializes the object and computes the
#' hash using the \link[digest]{digest} function in the digest package (algorithm xxhash32).
#'
#' hash_code is a generic function, so its default behavior can be overridden for a class.
#'
#' @param x object for which to compute the hash
#' @return an integer value
#' @export
hash_code <- function(x) {
    UseMethod('hash_code', x)
}

#' @export
hash_code.default <- function(x) {
  if (mode(x) == 'list') {
    hash_code.list(x)
  } else if (mode(x) == 'NULL') {
    197399964
  } else {
    hash_code_numeric(serialize(x, connection=NULL))
  }
}

combine_hash <- cppFunction('int32_t combine_hash(const NumericVector x) {
                              if (x.size() == 0) return 0;
                              int32_t h = 0;
                              for (int i = 0; i < x.size(); i++)
                                h += (int32_t) x[i] * 31;
                              return h;
                            }
                            ',
                               includes=c('#include <stdint.h>'))

hash_code_numeric <- cppFunction('int32_t hash_i(const NumericVector x) {
                                    int32_t seed = x.size();

                                    for (int i = 0; i < x.size(); i++) {
                                      const float xi = x[i];
                                      int32_t ui;
                                      memcpy(&ui, &xi, sizeof(double));
                                      seed = seed + xi * 31;
                                    }

                                    return seed;
                                  }', includes=c('#include <stdint.h>'))

.Unset <- new.env()

#' @export
hash_code.numeric <- function(x) {
  combine_hash(c(hash_code_numeric(x), hash_code(names(x))))
}

#' @export
hash_code.logical <- function(x) {
  combine_hash(c(hash_code_numeric(as.numeric(x)), hash_code(names(x))))
}

#' @export
hash_code.factor <- hash_code.logical

#' @export
hash_code.character <- function(x) {
  combine_hash(c(purrr::map_int(x, function(y) hash_code.numeric(charToRaw(y))),
                 hash_code(names(x))))
}

#' @export
hash_code.list <- function(x) {
  combine_hash(c(purrr::map_int(x, function(y) hash_code(y)),
                hash_code(names(x))))
}

#' @export
hash_code.matrix <- function(x) {
  combine_hash(c(hash_code.numeric(x), hash_code(rownames(x)), hash_code(colnames(x))))
}

#' @export
hash_code.data.frame <- hash_code.list

setClass('hash', representation(.data='list', .immutable='logical', .type='character'))

#' @export
hash <- function(..., capacity=1024) {
  args <- list(...)
  if (!any(purrr::map_lgl(args, purrr::is_formula)))
    stop('hash expects formulas of the form key ~ value.')
  keys <- purrr::map(args, lazyeval::f_eval_lhs)
  values <- purrr::map(args, lazyeval::f_eval_rhs)
  make_hash(keys, values, capacity)
}

resize <- function(hash, newcapacity) {
  make_hash_(keys(hash), values(hash), capacity=newcapacity, like_hash=hash)
}

#' @export
hash_from <- function(keys, fn) {
  fn <- purrr::as_function(fn)
  make_hash(keys, map(keys, fn))
}

#' @export
map_hash <- function(hash, .f, ...) {
  make_hash_(keys(hash), map2(keys(hash), values(hash), .f, ...), like=hash)
}

#' @export
keep_hash <- function(hash, .p, ...) {
  keys <- map2_lgl(keys(hash), values(hash), .p, ...)
  hash[keys(hash)[keys]]
}

#' @export
discard_hash <- function(hash, .p, ...) {
  keys <- map2_lgl(keys(hash), values(hash), .p, ...)
  hash[keys(hash)[!keys]]
}

#' @export
compact_hash <- function(hash) {
  discard_hash(dict, function(k, v) is.null(v))
}

make_hash_ <- function(keys, values, capacity=512, like_hash=NULL) {
  hash <- new('hash',
              .data = rep(list(list(k=vector('list', 1024), v=vector('list', 1024), occ=0)), capacity),
              .immutable = FALSE,
              .type = 'dictionary')

  if (!is.null(like_hash)) {
    hash@.immutable <- like_hash@.immutable
    hash@.type <- like_hash@.type
  }

  for (i in seq_along(keys))
    hash[[keys[[i]]]] <- values[[i]]

  hash
}

#' @export
make_hash <- function(keys, values, capacity=512) {
  make_hash_(keys, values, capacity)
}

#' @export
keys.hash <- function(x) {
  map(x@.data, function(bucket) if (bucket$occ > 0) bucket$k[1:bucket$occ]) %>%
    flatten() %>%
    compact()
}

#' @export
`keys.hash<-` <- function(...) {
  stop('This operation in supported only on ordered dictionaries.')
}

#' @export
values.hash <- function(x) {
  map(x@.data, function(bucket) if (bucket$occ > 0) bucket$v[1:bucket$occ]) %>%
    compact() %>%
    flatten()
}

#' @export
`values.hash<-` <- function(...) {
  stop('This operation in supported only on ordered dictionaries.')
}

#' @export
as.list.hash <- function(x, keys_as_strings=FALSE) {
  stop('Use either keys(), values() or entries() to convert a hash dictionary to a list.')
}

#' @export
`==.hash` <- function(hash1, hash2) {
  if (! is_hash(hash2))
    stop('Cannot compare a hash to a non-hash dictionary.')

  h1 <- hash_from(keys(hash1), function(key) {
    identical(hash1[[key]], hash2[[key]])
  })

  for (key in keys(hash2))
    h1[[k2]] <- identical(hash1[[key]], hash2[[key]])

  h1
}

#' @export
has.hash <- function(hash, key) {
  default(hash) <- .Unset
  !identical(hash[[key]], .Unset)
}

#' @export
invert.hash <- function(hash) {
  for (i in seq_along(hash@.data)) {
    k <- hash@.data[[i]]$k
    hash@.data[[i]]$k <- hash@.data[[i]]$v
    hash@.data[[i]]$v <- k
  }
  hash
}

#' @export
length.hash <- function(hash) {
  sum(purrr::map_dbl(hash@.data, function(bucket) bucket$occ))
}

#' @export
print.hash <- function(object) {
  cat(sprintf('%s %s with %d entries.\n',
                  if (object@.immutable) 'Immutable hash' else 'Hash',
                  object@.type,
                  length.hash(object)))

  type <- object@.type
  n <- 0

  for (bucket in object@.data) {
    if (bucket$occ > 0) {
      for (i in 1:bucket$occ) {
        cat('\n{key}\n')
        print(bucket$k[[i]])

        if (type == 'dictionary') {
          cat('{value}\n')
          print(bucket$v[[i]])
        }
        n <- n + 1
        if (n > getOption('max.print'))
          break
      }
      if (n > getOption('max.print'))
        break
    }
  }
}

setMethod("show", "hash", print.hash)

#' @export
`[[<-.hash` <- function(hash, key, value) {
  if (hash@.immutable)
    stop('Attempting to mutate an immutable hash.')
  hc <- hash_code(key) %% length(hash@.data) + 1

  occ <- hash@.data[[hc]]$occ

  if (occ > 0) {
    for (i in 1:occ) {
      if (identical(hash@.data[[hc]]$k[[i]], key)) {
        hash@.data[[hc]]$v[[i]] <- value
        return(hash)
      }
    }
  }
  hash@.data[[hc]]$k[[occ+1]] <- key
  hash@.data[[hc]]$v[[occ+1]] <- value
  hash@.data[[hc]]$occ <- occ + 1

  hash
}

#' @export
`$<-.hash` <- `[[<-.hash`

#' @export
`[[.hash` <- function(hash, key) {
  hc <- hash_code(key) %% length(hash@.data) + 1

  occ <- hash@.data[[hc]]$occ
  if (occ > 0) {
    for (i in 1:occ) {
      if (identical(hash@.data[[hc]]$k[[i]], key))
        return(hash@.data[[hc]]$v[[i]])
    }
  }
  return(attr(hash, 'default'))
}

#' @export
`$.hash` <- `[[.hash`

#' @export
`[.hash` <- function(hash, keys) {
  h <- hash_from(keys, function(key) hash[[key]])
  h@.immutable <- hash@.immutable
  h@.type <- hash@.type
  h
}

#' @export
`[.hash<-` <- function(hash, keys, value) {
  if (length(value) == 1)
    value <- rep(list(value), length(keys))

  for (i in seq_along(keys))
    hash[[keys[[i]]]] <- value[[i]]
  hash
}

#' @export
is_hash <- function(hash) {
  inherits(hash, 'hash')
}

#' @export
omit.hash <- function(hash, ..., omit=list(...)) {
  cap <- length(hash@.data)
  for (key in omit) {
    hc <- hash_code(key) %% cap + 1
    occ <- hash@.data[[hc]]$occ

    if (occ == 1) {
      hash@.data[[hc]]$occ <- 0
      hash@.data[[hc]]$k[[1]] <- NULL
      hash@.data[[hc]]$v[[1]] <- NULL
    } else if (occ > 1) {
      found <- 0
      for (i in 1:occ) {
        if (identical(hash@.data[[hc]]$k[[i]], key)) {
          found <- i
          break
        }
      }

      if (found > 0) {
        hash@.data[[hc]]$k <- hash@.data[[hc]]$k[-found]
        hash@.data[[hc]]$v <- hash@.data[[hc]]$v[-found]
        hash@.data[[hc]]$occ <- occ -1
      }
    }
  }

  hash
}

#' @export
immutable.hash <- function(hash) {
  hash@.immutable <- TRUE
  hash
}

#' @export
is_immutable.hash <- function(hash) {
  hash@.immutable
}

#' @export
as_hash <- function(list) {
  if (is_hash(list))
    list
  else
    make_hash(names(list), list)
}

#' @export
hashset <- function(..., items=list(...), capacity=1024) {
  h <- make_hash_(items, rep(TRUE, length(items)), capacity=capacity)
  h@.type <- 'set'
  h
}

#' @export
is_hashset <- function(set) {
  is_hash(set) && set@.type == 'set'
}

#' @export
as_hashset <- function(list) {
  if (is_hash(list)) {
    list@.type <- 'set'
    list
  } else {
    hash_set(items=list)
  }
}

#' @export
intersect <- function(x, y) {
  UseMethod('intersect')
}

intersect.default <- base::intersect

#' @export
intersect.hash <- function(x, y) {
  y <- as_hashset(y)
  keep_hash(x, ~ has(y, .x))
}

#' @export
union <- function(x, y) {
  UseMethod('union')
}

#' @export
union.hash <- function(x, y) {
  y <- as_hashset(y)

}

union.default <- base::union

#' @export
setdiff <- function(x, y) {
  UseMethod('setdiff')
}

#' @export
setdiff.hash <- function(x, y) {
  y <- as_hashset(y)
  hash_set(setdiff(keys(x), keys(y)))
}

setdiff.default <- base::setdiff

#' @export
setequal <- function(x, y) {
  UseMethod('setequal')
}

#' @export
setequal.hash <- function(x, y) {
  y <- as_hashset(y)
  setequal(keys(x), keys(y))
}

setequal.default <- base::setequal
