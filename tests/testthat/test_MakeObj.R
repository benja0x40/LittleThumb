# > MakeObj ====================================================================
context("MakeObj")

TestObject <- function(x) {
  r <- NULL
  if(x == 1) r <- list()
  if(x == 2) r <- new.env()
  r
}

# + Basics ---------------------------------------------------------------------
test_that("Basics", {

  f <- PathToRDS("x")
  y <- 1:10

  expect_false(exists("x"))
  expect_false(file.exists(f))

  expect_message(MkObj(x, { x <- 1:10 }), regexp = "save")

  expect_true(file.exists(f))
  expect_true(exists("x"))
  expect_identical(x, y)

  expect_message(MkObj(x, { x <- 1:10 }), regexp = "bypass")

  rm(x)
  expect_message(MkObj(x, { x <- 1:10 }), regexp = "load")
  expect_identical(x, y)

  rm(x, y)
  expect_message(
    MakeObj(x, rebuild = TRUE, cleanup = FALSE, { x <- y <- 0 }),
    regexp = "overwrite"
  )
  expect_true(exists("x"))
  expect_true(exists("y"))

  rm(x, y)
  expect_message(
    MakeObj(x, rebuild = TRUE, cleanup = TRUE, { x <- y <- 0 }),
    regexp = "overwrite"
  )
  expect_true(exists("x"))
  expect_false(exists("y"))

  expect_true(file.remove(f))

  f <- MkObj(x, { x <- 1:10 })
  expect_identical(f, PathToRDS("x"))

  DeleteObj(x)

  # Cleanup
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})

# + Advanced/1 -----------------------------------------------------------------
test_that("Advanced/1", {

  LittleThumb(rebuild = TRUE, reload = TRUE)

  expect_error(MakeObj(tst, { x <- 1 }), regexp = "does not define object")

  for(loop in 1:2) {

    # Top level
    tst <- TestObject(loop)
    RegisterObject("tst")
    MakeObj(z, parent.name = "tst", { z <- 1 })
    expect_identical(tst$z, 1)
    DeleteObj(z, parent = tst)
    expect_null(tst$z)

    # 1 level
    MakeObj(tst, {
      tst <- TestObject(loop)
      MakeObj(z, parent = tst, { z <- loop })
    })
    expect_identical(tst$z, loop)
    DeleteObj(tst)

    MakeObj(tst, {
      tst <- TestObject(loop)
      MakeObj(z, parent.name = "tst", { z <- loop })
    })
    expect_identical(tst$z, loop)
    DeleteObj(tst)
  }

  # Nested lists and environments
  MakeObj(x, {
    x <- list()
    MakeObj(y, parent = x, {
      y <-  new.env()
      MakeObj(z, parent = y, {
        z <- 0
      })
    })
  })
  expect_identical(x$y$z, 0)
  DeleteObj(x)

  MakeObj(x, {
    x <- new.env()
    MakeObj(y, parent = x, {
      y <-  list()
      MakeObj(z, parent = y, {
        z <- 0
      })
    })
  })
  expect_identical(x$y$z, 0)
  DeleteObj(x)

  # Cleanup
  unlink("_components_", recursive = T)
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})


# + Advanced/2 -----------------------------------------------------------------
test_that("Advanced/2", {

  LittleThumb(rebuild = FALSE, reload = TRUE)

  for(loop in 1:2) {
    xpr <- expression(
      MakeObj(tst, {
        tst <- TestObject(loop)
        MakeObj(z, parent = tst, { z <- loop })
      })
    )
    eval(xpr)
    expect_identical(tst$z, loop)
    DeleteObj(tst)
    expect_message(eval(xpr), regexp = "load")
    expect_identical(tst$z, loop)
    expect_message(DeleteObj(z, parent = tst), regexp = "delete")
    expect_message(DeleteObj(tst), regexp = "delete")
  }

  # Cleanup
  unlink("_components_", recursive = T)
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})
