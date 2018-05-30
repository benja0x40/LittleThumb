# > Arguments ==================================================================
context("Arguments")

# + DefaultArgs -----------------------------------------------------------------
test_that("DefaultArgs", {

  cfg <- list(x = 1, y = 2, z = 3)

  env <- new.env()
  DefaultArgs(cfg, to = env)
  expect_identical(as.list(env), cfg)

  env$y <- 0
  DefaultArgs(cfg, to = env)
  expect_identical(env$y, 0)

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
