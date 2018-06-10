# > MakeObj ====================================================================
context("MakeObj")

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

# + Advanced -------------------------------------------------------------------
test_that("Advanced", {

  LittleThumb(rebuild = TRUE, reload = TRUE)

  expect_error(MakeObj(tst, { x <- 1 }), regexp = "does not define object")

  RegisterObject("tst")
  tst <- new.env()
  MakeObj(z, parent = tst, { z <- 1 })
  expect_identical(tst$z, 1)
  DeleteObj(z, parent = tst)
  expect_null(tst$z)

  RegisterObject("tst")
  tst <- list()
  MakeObj(z, parent = tst, { z <- 1 })
  expect_identical(tst$z, 1)
  DeleteObj(z, parent = tst)
  expect_null(tst$z)

  # Parent = environment
  MakeObj(tst, {
    tst <- new.env()
    MakeObj(z, parent = tst, { z <- 1 })
  })
  expect_identical(tst$z, 1)
  DeleteObj(tst)

  MakeObj(tst, {
    tst <- new.env()
    MakeObj(z, parent.name = "tst", { z <- 2 })
  })
  expect_identical(tst$z, 2)
  DeleteObj(tst)

  # Parent = list object
  MakeObj(tst, {
    tst <- list()
    MakeObj(z, parent = tst, { z <- 3 })
  })
  expect_identical(tst$z, 3)
  DeleteObj(tst)

  MakeObj(tst, {
    tst <- list()
    MakeObj(z, parent.name = "tst", { z <- 4 })
  })
  expect_identical(tst$z, 4)
  DeleteObj(tst)

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
