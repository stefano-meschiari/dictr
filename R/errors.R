assert_valid_keys <- function(keys) {
  if (is.null(keys)) {
    stop_no_names()
  }
  if (any(is.na(keys))) {
    stop_na_in_keys(keys)
  }
  if (length(unique(keys)) != length(keys)) {
    stop_non_unique_keys(keys)
  }
}

stop_no_names <- function() {
  stop("Object does not have names")
}

stop_non_unique_keys <- function(keys) {
  non_unique <- table(keys, useNA = "ifany")
  non_unique <- non_unique[non_unique > 1]

  stop("Non-unique key(s) detected: ",
       stringr::str_c(names(non_unique), sep=", "))
}

stop_na_in_keys <- function(keys) {
  sum_na <- sum(is.na(keys))
  stop("NA keys are not valid (", sum_na, " NA key(s) detected).")
}

assert_keys_and_values_same_length <- function(keys, values) {
  if (length(keys) != length(values)) {
    stop(sprintf("Length of keys [%d] different from length of values [%d]",
                 length(keys), length(values)))
  }
}
