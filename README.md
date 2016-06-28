`niceR` makes the R language a bit nicer by adding a few conveniences to the base language and a few new convenience functions and data structures.

Install the package using [devtools](http://cran.r-%20project.org/package=devtools):

``` r
devtools::install_github('stefano-meschiari/nicer')
```

and import the library with `library`:

``` r
library(nicer)
```

The current version of `niceR` offers the following features:

-   a proper dictionary class with character keys, called `dict`.
-   a typed structure class, called `struct`. Constraints can also be applied on fields.
-   `+` works as a string concatenation operator, `%%` as a string formatting operator.

A proper dictionary class
=========================

The new class `dict` can be used to represent a heterogeneous dictionary with character keys. `dict`s have a few advantages over named lists:

-   Every value is uniquely associated with a key (no holes).
-   Keys are unique and cannot be repeated.
-   Printing of dicts is more compact.
-   Keys are never partially matched (a key named `test` will not match with `t` or `te`).
-   A dict can have default values for non-existing keys.

Creating a dictionary
---------------------

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

Accessing entries
-----------------

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
mapkv(d, function(k, v) paste(k, as.character(v)))

# Returns a dict with the same keys and squared values
d2 <- dict(a=1, b=2, c=3)
mapkv_dict(d, function(k, v) v^2)
```

Setting entries
---------------

Entries can be set just like regular lists:

``` r
d <- dict(first='Harrison', last='Solo')  # first=Harrison, last=Solo
d$last <- 'Ford'                          # first=Harrison, last=Ford
d[['first']] <- 'Leia'                    # first=Leia, last=Ford
d[c('last', 'title')] <- c('Organa', 'Princess') # first=Leia, last=Organa, title=Princess
```

Setting an entry to `NULL` does *not* delete the entry, but instead sets the entry to `NULL`. To delete one or more entries, use the `omit` function:

``` r
d <- dict(a=1, c=3)
d$b <- NULL         # a=1, b=NULL, c=3
d <- omit(a, 'a')   # b=NULL, c=3
```

Utilities
---------

A few utility functions inspired by the Underscore library:

-   `invert(dict)` returns a dictionary where keys and values are swapped.
-   `has(dict, key)` returns TRUE if `dict` contains `key`.
-   `omit(dict, key1, key2, ...)` returns a new dictionary omitting all the specified keys.
-   `extend(dict, dict1, ...)` copies all entries in `dict1` into `dict`, overriding any existing entries and returns a new dictionary.
-   `defaults(dict, defaults)` fill in entries from `defaults` into `dict` for any keys missing in `dict`.

Default dictionaries
--------------------

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

Strict dictionaries
-------------------

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

Typed structures (structs)
==========================

A `struct` object is similar to a `dict` object. An important difference is that it can specify type constraints (or arbitrary constraints) that will be enforced at runtime.

A `struct` is useful for:

-   when you need to impose types on values;
-   when you want to concisely define a function with strictly typed parameters;
-   when you want to concisely specify constraints on the fields of an object.

Creating structs
----------------

The fields of a new struct can be defined as follows:

``` r
# Struct definition
Person <- struct('Person', 
                 first_name ~ character,   # Must be character
                 last_name ~ character,    # Must be character
                 salary ~ numeric,         # Must be numeric
                 age ~ integer,            # Must be integer
                 data ~ any)               # Any value (including NULL)
```

The first argument is the name of the `struct` (this will also be its class); The value after the tilde (`~`) determines the type/class of the field. Typical values for the type are `numeric`, `complex`, `character`, `logical`, `integer`, `data.frame`, `list`, `lm`, `dict`, etc. The special type `any` indicates the field can accept any type. `NULL` is accepted only for fields of type `any`.

`struct` returns a constructor function that can be used to create objects conforming to the definition:

``` r
# Struct creation; use the Person() function to create new structs.
mary_smith <- Person(first_name = 'Mary',
                     last_name = 'Roosevelt',
                     salary = 100000,
                     age = 35L)

# Structs can extend other structs
Dog <- struct('Dog',
              name ~ character,
              owner ~ Person)
```

If no default values for the field are specified (see below), all fields must be set to a value.

Any attempt to create a struct, or set a field, with the wrong type of input data will cause a runtime error.

``` r
strange_employee <- Person(first_name = 100) # Will fail
```

You can also set default values for the fields of a struct:

``` r
Model <- struct('Model', 
                name = 'glm' ~ character,
                base_auc = 0.5 ~ numeric,
                family = 'binomial',          # type not specified, inferred to be character
                author = 'Chief Data Scientist')

model <- Model()   # creates a new Model struct with default values
model <- Model(name='part')  # overrides the default value for the field 'name'
```

If a type is not specified, it is inferred to be the class of the default value.

To describe the layout of a struct, use `describe`:

``` r
describe(mary_smith)
```

    ## Definition of struct of class: Person 
    ## 
    ## Fields:
    ## $ first_name  ~  character 
    ## $ last_name  ~  character 
    ## $ salary  ~  numeric 
    ## $ age  ~  integer 
    ## $ data  ~  any

'either' types
--------------

``` r
Rank <- struct('Rank',
               name ~ character,
               ranking ~ either(character, integer)) 
```

You can specify a field to take one of the specified types.

Constraints
-----------

Instead of specifying a type, you can specify a *constraint* for the value of a field. The constraint can be any expression that returns TRUE or FALSE.

To specify a constraint, supply an expression to the right hand side of `~` during the definition of the struct:

``` r
PositiveVector <- struct('PositiveVector',
                         x = 1 ~ { x > 0 },
                         y = 1 ~ { y > 0 },
                         z = 1 ~ { z > 0 })
v <- PositiveVector(x = 2, y = 3, z = 4)      # OK
v_bad <- PositiveVector(x = -2, y = 3, z = 4) # NOT OK
```

Expressions can include multiple variables:

``` r
UnitVector <- struct('UnitVector',
                     x = 1/sqrt(2) ~ { all.equal(sqrt(x^2 + y^2), 1) },
                     y = 1/sqrt(2) ~ { all.equal(sqrt(x^2 + y^2), 1) })

u <- UnitVector(x = 0, y = 1)    # OK
u2 <- UnitVector(x = 2, y = 3)   # Will fail
```

Finally, expressions can be used to enforce enumerations:

``` r
Student <- struct('Student',
                  subject ~ { subject %in% c('math', 'history') },
                  grade ~ { grade %in% c('A', 'B', 'C', 'D', 'F') },
                  name ~ character)

student <- Student(subject='math', grade='A', name='Mark')               # OK
bad_student <- Student(subject='physics', grade='unknown', name='Mark')  # Will fail
```

String concatenation using `+`
==============================

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
============================

The `%%` operator can be used in place of `sprintf`. The right operand should be a vector or list.

``` r
print('New random number: %.2f' %% runif(1))
```

    ## [1] "New random number: 0.94"

``` r
name <- 'Stefano'
last_name <- 'Meschiari'
print('Welcome, %s %s' %% c(name, last_name))
```

    ## [1] "Welcome, Stefano Meschiari"
