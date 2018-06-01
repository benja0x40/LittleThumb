# > Arguments ==================================================================
context("Arguments")

# + DefaultArgs -----------------------------------------------------------------
test_that("DefaultArgs", {

  cfg <- list(x = 1, y = 2, z = 3)

  src <- new.env()
  dst <- new.env()

  DefaultArgs(cfg, from = src)
  expect_identical(names(src), character(0))

  DefaultArgs(cfg, to = dst)
  expect_identical(as.list(dst), cfg)

  dst <- new.env()
  dst$y <- 0
  DefaultArgs(cfg, to = dst)
  expect_identical(as.list(dst), list(x = 1, y = 0, z = 3))

  alt <- list(x = 3, y = 2, z = 1)
  dst <- as.environment(alt)

  DefaultArgs(cfg, to = dst)
  expect_identical(as.list(dst, sorted = TRUE), alt)

  src <- new.env()
  src$y <- 0

  DefaultArgs(cfg, from = src, to = dst)
  expect_identical(as.list(dst, sorted = TRUE), alt)

  dst <- new.env()
  DefaultArgs(cfg, from = src, to = dst)
  expect_identical(as.list(dst), list(x = 1, y = 0, z = 3))

  f <- function(x = NULL, y = NULL, z = NULL, ...) {
    DefaultArgs(cfg)
    list(x = x, y = y, z = z)
  }

  expect_identical(f(), cfg)
  expect_identical(f(x = 0)$x, 0)
  expect_identical(f(y = 0)$y, 0)
  expect_identical(f(z = 0)$z, 0)
  expect_identical(f(i = 1:10), cfg)

  f <- function(x = NULL, y = NULL, z = NULL, ...) {
    DefaultArgs(cfg, ignore = "...", from = f)
    list(x = x, y = y, z = z)
  }

  expect_identical(f(), cfg)
  expect_identical(f(x = 0)$x, 0)
  expect_identical(f(y = 0)$y, 0)
  expect_identical(f(z = 0)$z, 0)
  expect_identical(f(i = 1:10), cfg)

})

# + ClonalArg ------------------------------------------------------------------
test_that("ClonalArg", {

  a <- c("x", "y")

  d <- "i"
  v <- "j"
  r <- ClonalArg(u = v, a, d)
  expect_identical(r, list(x = v, y = v))

  d <- LETTERS[1:5]
  v <- LETTERS[5:1]
  r <- ClonalArg(u = v, a, d)
  expect_identical(r, list(x = v, y = v))

  d <- matrix("I", 2, 2)
  v <- matrix("J", 2, 2)
  r <- ClonalArg(u = v, a, d)
  expect_identical(r, list(x = v, y = v))

  d <- 1
  v <- 0
  r <- ClonalArg(u = 0, a, d)
  expect_identical(r, list(x = v, y = v))

  r <- ClonalArg(u = c(x = 0), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = c(y = 0), a, d)
  expect_identical(r, list(x = d, y = v))

  r <- ClonalArg(u = c(x = v, y = v), a, d)
  expect_identical(r, list(x = v, y = v))

  r <- ClonalArg(u = list(x = v), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = list(y = v), a, d)
  expect_identical(r, list(x = d, y = v))

  r <- ClonalArg(u = list(x = v, y = v), a, d)
  expect_identical(r, list(x = v, y = v))

  d <- 1:2
  v <- c(0, 0)

  r <- ClonalArg(u = 0, a, d)
  expect_identical(r, list(x = v, y = v))

  r <- ClonalArg(u = c(x = 0), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = c(y = 0), a, d)
  expect_identical(r, list(x = d, y = v))

  r <- ClonalArg(u = list(x = v), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = list(y = v), a, d)
  expect_identical(r, list(x = d, y = v))

  d <- matrix(1, 2, 2)
  v <- matrix(0, 2, 2)

  r <- ClonalArg(u = 0, a, d)
  expect_identical(r, list(x = v, y = v))

  r <- ClonalArg(u = c(x = 0), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = c(y = 0), a, d)
  expect_identical(r, list(x = d, y = v))

  r <- ClonalArg(u = list(x = v), a, d)
  expect_identical(r, list(x = v, y = d))

  r <- ClonalArg(u = list(y = v), a, d)
  expect_identical(r, list(x = d, y = v))

})

# + LogicalArg -----------------------------------------------------------------
test_that("LogicalArg", {

  expect_true(LogicalArg("x", TRUE) == TRUE)
  expect_true(LogicalArg("x", FALSE) == FALSE)

  expect_true(LogicalArg("x", "x") == TRUE)
  expect_true(LogicalArg("x", "y") == FALSE)
  expect_true(LogicalArg("x", c("x", "y")) == TRUE)
  expect_true(LogicalArg("x", c("y", "z")) == FALSE)

  expect_true(LogicalArg("x", c(x = TRUE)) == TRUE)
  expect_true(LogicalArg("x", c(x = FALSE)) == FALSE)
  expect_true(LogicalArg("x", c(x = TRUE, y = FALSE)) == TRUE)
  expect_true(LogicalArg("x", c(x = FALSE, y = TRUE)) == FALSE)
  expect_true(LogicalArg("x", c(y = TRUE, z = TRUE)) == FALSE)

  expect_true(LogicalArg("x", list(x = TRUE)) == TRUE)
  expect_true(LogicalArg("x", list(x = FALSE)) == FALSE)
  expect_true(LogicalArg("x", list(x = TRUE, y = FALSE)) == TRUE)
  expect_true(LogicalArg("x", list(x = FALSE, y = TRUE)) == FALSE)
  expect_true(LogicalArg("x", list(y = TRUE, z = TRUE)) == FALSE)

})

# + NamedArg -------------------------------------------------------------------
test_that("NamedArg", {

  x <- "X3"
  a <- LETTERS[1:5]

  expect_identical(NamedArg(x, a), a)

  names(a) <- paste0("X", 1:5)
  expect_identical(NamedArg(x, a), a[x])

  a <- as.list(a)
  expect_identical(NamedArg(x, a), a[[x]])

})

# + ObjWithExpressionArgs ------------------------------------------------------
test_that("ObjWithExpressionArgs", {

  x <- NULL
  a <- quote(MakeObj())
  expect_error(ObjWithExpressionArgs(a, x))

  x <- NULL
  a <- quote(MakeObj(u))
  expect_error(ObjWithExpressionArgs(a, x))

  x <- NULL
  a <- quote(MakeObj({ u <- FALSE }))
  expect_error(ObjWithExpressionArgs(a, x))

  x <- NULL
  a <- quote(MakeObj(u = v, { u <- FALSE }))
  expect_error(ObjWithExpressionArgs(a, x))

  x <- NULL
  a <- quote(MakeObj(u, v = w))
  expect_error(ObjWithExpressionArgs(a, x))

  x <- NULL
  a <- quote(MakeObj(obj, { obj <- FALSE }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj"))

  x <- NULL
  a <- quote(MakeObj(obj, rebuild = TRUE, { obj <- FALSE }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj", rebuild = TRUE))

  x <- NULL
  a <- quote(MakeObj(name = "obj", { obj <- FALSE }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(as.list(a), list(name = "obj"))

  x <- NULL
  a <- quote(MakeObj(name = "obj", rebuild = TRUE, { obj <- FALSE }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(rebuild = TRUE, name = "obj"))

  x <- NULL
  a <- quote(MakeObj(rebuild = TRUE, name = "obj", { obj <- FALSE }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj", rebuild = TRUE))

  x <- NULL
  a <- quote(MakeObj(zzz, name = "obj", { obj <- FALSE }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj"))

  x <- NULL
  a <- quote(MakeObj(zzz, rebuild = TRUE, name = "obj", { obj <- FALSE }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj", rebuild = TRUE))

})
