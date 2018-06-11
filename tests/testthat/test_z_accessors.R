# > Accessors ==================================================================
context("Accessors")

TestObject <- function(x) {
  r <- NULL
  if(x == 1) r <- list()
  if(x == 2) r <- new.env()
  r
}

# + ObjectExists ---------------------------------------------------------------
test_that("ObjectExists", {

  expect_false(ObjectExists("v"))
  expect_false(ObjectExists("z", prn.name = "v"))

  v <- 1
  expect_true(ObjectExists("v"))

  for(loop in 1:2) {
    v <- TestObject(loop)
    v$x <- 1
    v$y <- 2
    expect_false(ObjectExists("z", prn.name = "v"))
    expect_false(ObjectExists("z", parent = v))

    expect_true(ObjectExists("x", prn.name = "v"))
    expect_true(ObjectExists("x", parent = v))
    expect_true(ObjectExists("x", prn.name = "v", parent = v))
  }

  # Cleanup
  rm(v)
})

# + GetValue -------------------------------------------------------------------
test_that("GetValue", {

  expect_null(GetValue("v"))
  expect_null(GetValue("z", prn.name = "v"))

  v <- 1
  expect_identical(GetValue("v"), v)

  for(loop in 1:2) {
    v <- TestObject(loop)
    v$x <- 1
    v$y <- 2
    expect_null(GetValue("z", prn.name = "v"))
    expect_null(GetValue("z", parent = v))

    expect_identical(GetValue("x", prn.name = "v"), v$x)
    expect_identical(GetValue("y", parent = v), v$y)
  }

  # Cleanup
  rm(v)
})

# + SetValue -------------------------------------------------------------------
test_that("SetValue", {

  for(loop in 1:2) {

    SetValue("v", value = TestObject(loop))
    expect_true(ObjectExists("v"))

    SetValue("x", parent = v, value = 1)
    expect_identical(ObjectExists("x", prn.name = "v"), loop == 2)
    expect_identical(ObjectExists("x", parent = v), loop == 2)

    SetValue("x", prn.name = "v", value = 1)
    SetValue("y", prn.name = "v", value = 2)

    expect_true(ObjectExists("x", prn.name = "v"))
    expect_true(ObjectExists("y", parent = v))

    expect_identical(GetValue("x", prn.name = "v"), 1)
    expect_identical(GetValue("y", parent = v), 2)

    SetValue("x", prn.name = "v", value = NULL)
    SetValue("y", prn.name = "v", value = NULL)

    expect_identical(ObjectExists("x", prn.name = "v"), loop == 2)
    expect_identical(ObjectExists("x", parent = v), loop == 2)
  }

  # Cleanup
  rm(v)
})

# + RemoveObject ---------------------------------------------------------------
test_that("RemoveObject", {

  v <- 1
  RemoveObject("v")
  expect_false(exists("v", inherits = FALSE))

  for(loop in 1:2) {
    v <- TestObject(loop)
    v$x <- 1
    v$y <- 2

    RemoveObject("x", prn.name = "v")
    RemoveObject("y", parent = v)

    expect_false(ObjectExists("x", prn.name = "v"))
    expect_identical(ObjectExists("y", parent = v), loop == 1)
  }

  # Cleanup
  rm(v)
})
