`niceR` makes the R language a bit nicer by adding a few conveniences to the base language and a few new convenience functions and data structures.

Install the package using [devtools](http://cran.r-%20project.org/package=devtools):

``` r
devtools::install_github('stefano-meschiari/nicer')
```

and import the library with `library`:

``` r
library(nicer)
```

License is MIT.

NiceR features
==============

The current version of `niceR` offers the following features:

-   `+` works as a string concatenation operator, `%%` as a string formatting operator.
-   a proper dictionary class with character keys, called `dict`.

A proper dictionary class
-------------------------

The new class `dict` can be used to represent a heterogeneous dictionary with character keys. `dict`s have a few advantages over named lists:

-   Every value is uniquely associated with a key (no holes).
-   Keys are unique and cannot be repeated.
-   Printing of dicts is more compact.
-   Keys are never partially matched (a key named `test` will not match with `t` or `te`).
-   A dict can have default values for non-existing keys.

### Creating a dictionary

The `dict` function can be used to create a new dictionary:

``` r
d <- dict(color='blue', pattern='solid', width=3)
```

You can create a dictionary out of a list of keys and values using `make_dict`:

``` r
d <- make_dict(keys=c('color', 'pattern', 'width'),
               values=c('blue', 'solid', 3))
```

You can convert a named list to a dictionary using `as.dict`:

``` r
d <- as.dict(list(color='blue', pattern='solid', width=3))
```

Printing looks nice:

``` r
print(d)
```

    ##   $ color : [1] "blue"
    ## $ pattern : [1] "solid"
    ##   $ width : [1] 3

### Accessing entries

You can access values using the familiar R syntax for lists:

``` r
d <- dict(color='blue', pattern='solid', width=3)

d$color                   # blue
d[['pattern']]            # solid
d[c('color', 'pattern')]  # a sub-dictionary with keys color and pattern
```

You can get keys and values using the `keys` and `values` functions:

``` r
keys(d)      # c('color', 'pattern', 'width')
values(d)    # list('blue', 'solid', 3)
```

You can get a list of "entries" (each element being a list with key and value). This is useful for iteration:

``` r
for (entry in entries(d)) {
  cat(entry$key, ' = ', entry$value)
}
```

You can use the `mapkv` and `mapkv_dict` to map over keys and values. These functions are equivalent to the `map` family of functions in [purrr](https://github.com/hadley/purrr).

``` r
# Returns a list
mapkv(d, function(k, v) paste(key, as.character(v)))
# Returns a dict with the same keys and squared values
d2 <- dict(a=1, b=2, c=3)
mapkv_dict(d, function(k, v) v^2)
```

### Utilities

A few utility functions inspired by the Underscore library:

-   `invert(dict)` returns a dictionary where keys and values are swapped.
-   `has(dict, key)` returns TRUE if `dict` contains `key`.
-   `omit(dict, key1, key2, ...)` returns a new dictionary omitting all the specified keys.
-   `extend(dict, dict1, ...)` copies all entries in `dict1` into `dict`, overriding any existing entries and returns a new dictionary.
-   `defaults(dict, defaults)` fill in entries from `defaults` into `dict` for any keys missing in `dict`.

### Default dictionaries

You can create a dictionary with default values using `default_dict`. Any time a non-existing key is accessed, the default value is returned.

``` r
salaries <- default_dict(employee_a = 50000,
                         employee_b = 100000,
                         default = 65000)
```

You can turn any dictionary into a default dictionary by setting its `default` attribute:

``` r
attr(salaries, 'default') <- 70000
```

### Strict dictionaries

You can create a strict dictionary using `strict_dict`. Any time a non-existing key is accessed, an exception is thrown using `stop()`.

``` r
# Associating each letter with a number
letters_to_numbers <- strict_dict(a=1, b=2, c=3, d=4) # etc.

# Accessing an existing key, that's OK!
print(letters_to_numbers$a)
```

    ## [1] 1

``` r
# Accessing a non-letter will throw!
tryCatch(letters_to_numbers$notaletter, error=function(e) print(e))
```

    ## <simpleError in `$.dict`(letters_to_numbers, notaletter): Attempted access of non-existing keynotaletter>

String concatenation using `+`
------------------------------

After you import `niceR`, you can concatenate strings with the regular `+` operator. The `+` operator has the same semantics as the `paste0` or `stringr::str_c` functions.

``` r
a <- 'My string'
b <- 'another string'
print(a + ' and ' + b)
```

    ## [1] "My string and another string"

``` r
nums <- c(exp(1), pi)
print('My favorite number is ' + nums)
```

    ## [1] "My favorite number is 2.71828182845905"
    ## [2] "My favorite number is 3.14159265358979"

String formatting using `%%`
----------------------------

The `%%` operator can be used in place of `sprintf`. The right operand should be a vector or list.

``` r
print('New random number: %.2f' %% runif(1))
```

    ## [1] "New random number: 0.91"

``` r
name <- 'Stefano'
last_name <- 'Meschiari'
print('Welcome, %s %s' %% c(name, last_name))
```

    ## [1] "Welcome, Stefano Meschiari"
