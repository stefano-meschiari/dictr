test_that("dict, default_dict, strict_dict, make_dict create dictionaries with the right properties", {
  x <- dict(a=1, b='character', c=list(), d=NULL)
  expect_equal(is_dict(x), TRUE)
  expect_equal(keys(x), c('a', 'b', 'c', 'd'))

  y <- default_dict(a=1, b='character', .default = 'default value')
  expect_equal(is_dict(y), TRUE)
  expect_equal(y$d, 'default value')

  z <- strict_dict(a=1, b='character')
  expect_equal(is_dict(z), TRUE)
  expect_error(z$c)

  w <- make_dict(keys = c('a', 'b', 'c', 'd'),
                 values = list(1, 'character', list(), NULL))
  expect_identical(x, w)
})

test_that("keys, values, length work correctly", {
  x <- dict(name='John', last_name='Smith')
  expect_identical(keys(x), c('name', 'last_name'))
  expect_identical(keys(x), names(x))
  expect_equal(length(x), 2)
  expect_identical(values(x), list('John', 'Smith'))
})

test_that("values equal to NULL can be set", {
  x <- dict()
  x$a <- NULL
  expect_identical(keys(x), 'a')
  expect_identical(values(x), list(NULL))

  x$b <- NULL
  x$c <- 1
  expect_identical(keys(x), c('a', 'b', 'c'))
  expect_identical(values(x), list(NULL, NULL, 1))

  x$c <- NULL
  expect_identical(values(x), list(NULL, NULL, NULL))
})

test_that("Empty string is a valid key", {
  x <- dict()
  x[[""]] <- 1
  expect_identical(x[[""]], 1)
  expect_identical(x[""], make_dict("", 1))

  x <- make_dict(c("a", "", "b"), c(1, 2, 3))
  expect_identical(x[c("a", "")], make_dict(c("a", ""), c(1, 2)))

  y <- omit(x, "")
  expect_identical(y, dict(a=1, b=3))
  y[""] <- "empty"
  expect_identical(y, make_dict(c("a", "b", ""), list(1, 3, "empty")))

  z <- c(dict(a=1), make_dict("", 2))
  expect_identical(z, make_dict(c("a", ""), list(1, 2)))
})

test_that('dollar, bracket and double bracket operators work correctly', {
  d <- dict(color='blue', pattern='solid', width=3, null=NULL)
  expect_identical(d$color, 'blue')
  expect_identical(d[['pattern']], 'solid')
  expect_identical(d[c('color', 'width')],
                   dict(color='blue', width=3))
  expect_identical(d[c('width', 'null')],
                   dict(width=3, null=NULL))

  d$color <- 'green'
  expect_identical(d$color, 'green')
  d[['origin']] <- 'center'
  expect_identical(d$origin, 'center')
  d$null2 <- NULL
  expect_identical(d$null2, NULL)

  # set multiple values at once
  d <- dict(pattern="dotted")
  d[c('pattern', 'new_key')] <- c('dashed', 'set')
  expect_identical(d[c('pattern', 'new_key')],
                   dict(pattern='dashed', new_key='set'))

  d <- dict(pattern="dotted")
  d['pattern', 'new_key'] <- c('dashed', 'set')
  expect_identical(d['pattern', 'new_key'],
                   dict(pattern='dashed', new_key='set'))
})

test_that('equality works as expected', {
  x <- dict(a=1, b=2, c=3, d=NULL)
  y <- dict(a=3, b=2, c=1, d=NULL)
  expect_identical(x == y,
                   c(a=FALSE, b=TRUE, c=FALSE, d=TRUE))

  # NULL values should compare equal
  z <- dict(a=1, b=2, c=NULL, d=NULL, e=10)
  expect_equal(x == z,
               c(a=TRUE, b=TRUE, c=FALSE, d=TRUE, e=NA))

  # equality between differently-ordered dicts
  x2 <- dict(b=2, d=NULL, a=1, c=3)
  expect_equal(x == x2,
               c(a=TRUE, b=TRUE, c=TRUE, d=TRUE))
})

test_that('can create dicts with implicit keys', {
  a <- 1
  b <- 2
  expect_identical(dict(a, b), dict(a=a, b=b))
})

test_that('omit removes keys', {
  x <- dict(a=1, b=2, c=3)
  y <- dict(a=1)
  expect_identical(omit(x, 'b', 'c'),
                   y)
})

test_that('extend and defaults work as specified', {
  x <- dict(a=1, b=2, d=4)
  y <- dict(a=10, b=20, c=30)

  expect_identical(extend(x, y), dict(a=10, b=20, d=4, c=30))
  expect_identical(defaults(x, y), dict(a=1, b=2, c=30, d=4))
})

test_that('purrr functions work as specified', {
  x <- dict(a=1, b=2, c=3, d=4)
  y <- dict(a=1, b=4, c=9, d=16)
  z <- dict(name='John', last_name='Smith', job=NULL)
  expect_identical(map_dict(x, ~ .y^2), y)

  expect_identical(keep_dict(y, ~ .y < 5), dict(a=1, b=4))
  expect_identical(discard_dict(y, ~ .y < 5), dict(c=9, d=16))
  expect_identical(compact_dict(z), dict(name='John', last_name='Smith'))
})

test_that('Matching is always exact', {
  x <- dict(my_long_key = 1)
  expect_null(x$my)
  expect_identical(x$my_long_key, 1)
})

test_that("Rename works as specified", {
  x <- dict(a=1, b=2, c=3)
  expect_identical(rename_keys(x, A='a'), dict(A=1, b=2, c=3))
})

test_that("Get or else works as specified", {
  x <- dict(a=1, b=NULL)
  expect_identical(get_or_else(x, "a", "default"), 1)
  expect_identical(get_or_else(x, "b", "default"), NULL)
  expect_identical(get_or_else(x, "c", "default"), "default")
})

test_that("make_dict with a function argument works as specified", {
  x <- c("a", "b")
  expect_identical(make_dict(x, ~ paste0(.x, .x)),
                   dict(a="aa", b="bb"))
})

test_that("Looping over entries works as specified", {
  x <- dict(a=1, b=2, c=3)
  filtered <- entries(x) %>%
    purrr::map(~ entry(.$key, .$value * 2)) %>%
    as_dict()

  expect_identical(filtered, dict(a=2, b=4, c=6))
})
