# > MakeObj ====================================================================
context("MakeObj")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  cfg <- LittleThumb() # Global options

  # LittleThumb(envir = sys.frame(sys.nframe()))

  f <- MakePath("x", ext = cfg$extension)
  y <- 1:10

  expect_false(exists("x"))
  expect_false(file.exists(f))

  expect_message(MakeObj(x, { x <- 1:10 }), regexp = "save")

  expect_true(file.exists(f))
  expect_true(exists("x"))
  expect_identical(x, y)

  expect_message(MakeObj(x, { x <- 1:10 }), regexp = "bypass")

  rm(x)
  expect_message(MakeObj(x, { x <- 1:10 }), regexp = "load")
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

  # Cleanup
  expect_true(file.remove(f))
})
