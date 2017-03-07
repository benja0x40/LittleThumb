# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# DEFINITION ===================================================================
context(.ttmsg.("LT_Data", "definition"))

# + new ------------------------------------------------------------------------
test_that("new", {

  r <- LT_Data()

  expect_s3_class(r, c("LT_Data", "list"))
  expect_true(inherits(r, "LT_Data"))
  expect_true(inherits(r, "list"))
})

# > TextRepresentation =========================================================
context(.ttmsg.("LT_Data", "> TextRepresentation"))

# + obj2txt & txt2obj ----------------------------------------------------------
test_that("obj2txt & txt2obj", {

  ltn <- "something"

  md <- LT_MetaData(a = 0:9)
  td <- LT_TabularData(v = 0:9)

  x <- LT_Data(MD = md, TD = td)
  y <- txt2obj(obj2txt(x))
  expect_is(y, "LT_Data")

  x <- LT_Data(name = ltn, MD = md, TD = td)
  y <- txt2obj(obj2txt(x))
  expect_identical(y$id, x$id)
  expect_identical(y$name, x$name)
  expect_identical(y$MD, x$MD)
  expect_identical(y$TD, x$TD)
  expect_identical(y, x)
})

# + lt_save & lt_load ----------------------------------------------------------
test_that("lt_save & lt_load", {

  md <- LT_MetaData(a = 0:9)
  td <- LT_TabularData(v = 0:9)

  x <- LT_Data(name = "somedata", MD = md, TD = mtcars)
  lt_save(x, "test.txt", rn = T)
  y <- lt_load("test.txt")
  file.remove("test.txt")
  expect_equal(y, x)
  expect_identical(y$id, x$id)
  expect_identical(y$name, x$name)
  expect_identical(y$MD, x$MD)
  expect_equal(y$TD, x$TD)
})
