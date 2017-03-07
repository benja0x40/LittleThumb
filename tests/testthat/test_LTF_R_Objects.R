# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# tools: data.frame ============================================================
context(.ttmsg.("> R Objects", "data.frame"))

# + rownames2col & col2rownames ------------------------------------------------
test_that("rownames2col & col2rownames", {

  x <- data.frame(hexa = c(0:9, LETTERS[1:6]), stringsAsFactors = F)
  y <- rownames2col(x, "rnc")
  expect_identical(y$rnc, as.integer(rownames(x)))

  y <- col2rownames(y, "rnc")
  expect_identical(rownames(y), rownames(x))
  expect_identical(y, x)

  x <- mtcars
  y <- rownames2col(x, "rnc")
  expect_identical(y$rnc, rownames(x))

  y <- col2rownames(y, "rnc")
  expect_identical(rownames(y), rownames(x))
  expect_identical(y, x)
})

# S3 Classes ===================================================================
context(.ttmsg.("> R Objects", "S3 class"))

# + add_class ----------------------------------------------------------------
test_that("add_class", {

  x <- add_class(integer(), "MyClass")
  expect_identical(class(x), c("MyClass", "integer"))

  x <- add_class(character(), "MyClass")
  expect_identical(class(x), c("MyClass", "character"))

  x <- add_class(list(), "MyClass")
  expect_identical(class(x), c("MyClass", "list"))

  x <- add_class(data.frame(), "MyClass")
  expect_identical(class(x), c("MyClass", "data.frame"))
})

# + check_class ----------------------------------------------------------------
test_that("check_class", {

  expect_true(check_class(NULL,   "NULL"))
  expect_true(check_class(NULL,   "NULL", lst = T))

  expect_false(check_class(NULL,   "list"))
  expect_false(check_class(NULL,   "list", lst = T))

  expect_true(check_class(integer(0), "integer"))
  expect_true(check_class(1:10, "integer"))
  expect_true(check_class(1:10, "integer", lst = T))

  expect_false(check_class(integer(0), "list"))
  expect_false(check_class(1:10, "list"))
  expect_false(check_class(1:10, "list", lst = T))

  expect_true(check_class(1:10, "integer"))
  expect_true(check_class(1:10, "numeric"))
  expect_true(check_class(1:10, c("integer", "numeric"), strict = T))
  expect_true(check_class(1:10, c("integer", "numeric"), strict = F))
  expect_true(check_class(1:10, c("list", "numeric"), strict = F))

  expect_false(check_class(1:10, "character"))
  expect_false(check_class(1:10, c("list", "numeric"), strict = T))

  expect_true(check_class(list(), "list"))
  expect_true(check_class(list(), "list", lst = T))

  expect_false(check_class(list(1, 2, 3), "list"))
})

# + reassign -------------------------------------------------------------------
test_that("reassign", {

  e1 <- new.env()
  e2 <- new.env()
  v <- "something"
  x <- 1:10
  expect_identical(ls(), c("e1", "e2", "v", "x" ))

  reassign(v, pos = e1, src = environment(), keep = F)
  reassign(x, pos = e2, src = environment(), keep = F)

  expect_identical(ls(e1), "v")
  expect_identical(ls(e2), "x")
  expect_identical(ls(), c("e1", "e2"))

  expect_identical(e1$v, "something")
  expect_identical(e2$x, 1:10)

  reassign("v", pos = e2, src = e1, keep = T)
  reassign("x", pos = e1, src = e2, keep = T)

  expect_identical(ls(e1), ls(e2))
  expect_identical(e1$x, 1:10)
  expect_identical(e2$v, "something")
})
