# > Arguments ==================================================================
context("Arguments")

TestObject <- function(x) {
  r <- NULL
  if(x == 1) r <- list()
  if(x == 2) r <- new.env()
  r
}

# + DefaultArgs ----------------------------------------------------------------
test_that("DefaultArgs", {

  cfg <- list(x = 1, y = 2, z = 3)

  for(loop in 1:2) {
    src <- TestObject(loop)
    dst <- TestObject(loop)

    DefaultArgs(cfg, from = src)
    expect_identical(as.character(names(src)), character(0))

    DefaultArgs(cfg, to = dst)
    expect_identical(as.list(dst), cfg)

    dst <- TestObject(loop)
    dst$y <- 0
    DefaultArgs(cfg, to = dst)
    expect_identical(as.list(dst)[c("x", "y", "z")], list(x = 1, y = 0, z = 3))
  }

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

# + VectorArgs -----------------------------------------------------------------
test_that("VectorArgs", {

  x <- 0
  y <- 1:10

  VectorArgs(c("x", "y"))
  expect_identical(x, rep(0, 10))
  expect_identical(y, 1:10)

  VectorArgs(c("x", "y"), size = 15)
  expect_identical(x, rep(0, 15))
  expect_identical(y[11:15], 1:5)

  a <- list(x = 0, y = 1:10)

  r <- VectorArgs(c("x", "y"), from = a)
  expect_identical(r$x, rep(0, 10))
  expect_identical(r$y, 1:10)

  r <- VectorArgs(c("x", "y"), from = a, size = 15)
  expect_identical(r$x, rep(0, 15))
  expect_identical(r$y[11:15], 1:5)

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

# LittleThumb ##################################################################

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

# + ManageObjectAndParentArgs --------------------------------------------------
test_that("ManageObjectAndParentArgs", {

  a <- quote(MyFunction())
  expect_error(ManageObjectAndParentArgs(a), regexp = "insufficient")

  a <- quote(MyFunction(x = 1))
  expect_error(ManageObjectAndParentArgs(a), regexp = "missing 'obj'")

  a <- quote(MyFunction(x))
  a <- ManageObjectAndParentArgs(a)
  expect_identical(a, quote(list(obj = x, name = "x")))

  a <- quote(MyFunction(x, name = "y"))
  a <- ManageObjectAndParentArgs(a)
  expect_identical(a, quote(list(obj = x, name = "y")))

  a <- quote(MyFunction(x, parent = i))
  expect_error(ManageObjectAndParentArgs(a), regexp = "unknown parent")

  a <- quote(MyFunction(x, parent.name = "j"))
  expect_error(ManageObjectAndParentArgs(a), regexp = "unknown parent")

  RegisterObject("i")
  RegisterObject("j")

  a <- quote(MyFunction(x, parent = i))
  a <- ManageObjectAndParentArgs(a)
  expect_identical(
    a, quote(list(obj = x, parent = i, name = "x", parent.name = "i"))
  )
  expect_identical(GetParents("x"), "i")

  a <- quote(MyFunction(x, parent.name = "j"))
  a <- ManageObjectAndParentArgs(a)
  expect_identical(
    a, quote(list(obj = x, parent.name = "j", name = "x"))
  )
  expect_identical(GetParents("x"), "j")

  a <- quote(MyFunction(x, parent = i, parent.name = "j"))
  a <- ManageObjectAndParentArgs(a)
  expect_identical(
    a, quote(list(obj = x, parent = i, parent.name = "j", name = "x"))
  )
  expect_identical(GetParents("x"), "j")

  # Cleanup
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})
