# =============================================================================.
context("LT_Description.R | functions")
# -----------------------------------------------------------------------------.
test_that("ndata", {
  x <- mtcars # Test data.frame
  a <- new("LT_Description", data = DataFrame(x))
  expect_identical(ndata(a), nrow(x))
})
# -----------------------------------------------------------------------------.
test_that("property", {
  k <- "somekind"
  v <- "something"
  a <- new("LT_Description")
  property(a, k) <- v
  expect_identical(v, property(a, k))
})
# -----------------------------------------------------------------------------.
test_that("properties", {
  a <- new("LT_Description")

  k <- "name"
  expect_identical(k, properties(a))

  k <- c(k, "somekind")
  v <- "something"
  property(a, k[2]) <- v
  expect_identical(k, properties(a))
})
# -----------------------------------------------------------------------------.
test_that("lt_name", {
  s <- "itself"
  a <- new("LT_Description")
  lt_name(a) <- s
  expect_identical(s, lt_name(a))
})
# -----------------------------------------------------------------------------.
test_that("obj2txtdef and txtdef2obj", {
  a <- new("LT_Description", data = DataFrame(mtcars))
  b <- txtdef2obj(obj2txtdef(a))
  expect_identical(a@id, b@id)
  expect_identical(a@properties, b@properties)
  expect_identical(as.matrix(a@data), as.matrix(b@data))
})
# -----------------------------------------------------------------------------.
test_that("check_txtdef", {
  a <- new("LT_Description", data = DataFrame(mtcars))

  x <- obj2txtdef(a)
  r <- check_txtdef(x)
  expect_identical(r, NULL)

  x <- obj2txtdef(a)
  x <- x[! grepl("^#", x, perl = T)]
  r <- check_txtdef(x)
  expect_identical(r, "missing header section")

  x <- obj2txtdef(a)
  x <- x[-2]
  r <- check_txtdef(x)
  expect_identical(r, "missing object statement")

  x <- obj2txtdef(a)
  x <- x[-3]
  r <- check_txtdef(x)
  expect_identical(r, "missing json section")
})
# -----------------------------------------------------------------------------.
test_that("save_description and load_description", {
  a <- new("LT_Description", data = DataFrame(mtcars))
  save_description(a, "test.txt")
  b <- load_description("test.txt")
  expect_identical(a@properties, b@properties)
  expect_identical(as.matrix(a@data), as.matrix(b@data))
})
# -----------------------------------------------------------------------------.
