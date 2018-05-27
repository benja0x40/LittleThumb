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

  expect_true(LogicalArg("x", T) == T)
  expect_true(LogicalArg("x", F) == F)

  expect_true(LogicalArg("x", "x") == T)
  expect_true(LogicalArg("x", "y") == F)
  expect_true(LogicalArg("x", c("x", "y")) == T)
  expect_true(LogicalArg("x", c("y", "z")) == F)

  expect_true(LogicalArg("x", c(x = T)) == T)
  expect_true(LogicalArg("x", c(x = F)) == F)
  expect_true(LogicalArg("x", c(x = T, y = F)) == T)
  expect_true(LogicalArg("x", c(x = F, y = T)) == F)
  expect_true(LogicalArg("x", c(y = T, z = T)) == F)

  expect_true(LogicalArg("x", list(x = T)) == T)
  expect_true(LogicalArg("x", list(x = F)) == F)
  expect_true(LogicalArg("x", list(x = T, y = F)) == T)
  expect_true(LogicalArg("x", list(x = F, y = T)) == F)
  expect_true(LogicalArg("x", list(y = T, z = T)) == F)

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
  a <- quote(MakeObj(obj, { obj <- F }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj"))

  x <- NULL
  a <- quote(MakeObj(obj, rebuild = T, { obj <- F }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj", rebuild = T))

  x <- NULL
  a <- quote(MakeObj(name = "obj", { obj <- F }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(as.list(a), list(name = "obj"))

  x <- NULL
  a <- quote(MakeObj(name = "obj", rebuild = T, { obj <- F }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(rebuild = T, name = "obj"))

  x <- NULL
  a <- quote(MakeObj(rebuild = T, name = "obj", { obj <- F }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj", rebuild = T))

  x <- NULL
  a <- quote(MakeObj(zzz, name = "obj", { obj <- F }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj"))

  x <- NULL
  a <- quote(MakeObj(zzz, rebuild = T, name = "obj", { obj <- F }))
  a <- as.list(ObjWithExpressionArgs(a, x))
  expect_true(is.language(x))
  expect_identical(a, list(name = "obj", rebuild = T))

})
