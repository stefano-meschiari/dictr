assert_unique_keys <- function(keys) {
  if (length(unique(keys)) != length(keys)) {
    stop_non_unique_keys(keys)
  }
}

stop_non_unique_keys <- function(keys) {
  non_unique <- table(keys)
  non_unique <- non_unique[non_unique > 1]

  stop("Non-unique keys detected: ",
       stringr::str_c(names(non_unique), sep=", "))
}

assert_keys_and_values_same_length <- function(keys, values) {
  if (length(keys) != length(values)) {
    stop(sprintf("Length of keys [%d] different from length of values [%d]",
                 length(keys), length(values)))
  }
}
