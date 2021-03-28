assert_unique_keys <- function(keys) {
  if (length(unique(keys)) != length(keys)) {
    stop_non_unique_keys(keys)
  }
}

stop_non_unique_keys <- function(keys) {
  non_unique <- setdiff(keys, unique(keys))
  stop("Non-unique keys detected: ",
       stringr::str_c(non_unique, sep=", "))
}
