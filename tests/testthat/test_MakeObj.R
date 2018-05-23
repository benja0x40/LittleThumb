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

  expect_message(MakeObj(x, { x <- 1:10 }), regexp = "saving")

  expect_true(file.exists(f))
  expect_true(exists("x"))
  expect_identical(x, y)

  expect_message(MakeObj(x, { x <- 1:10 }), regexp = "passing")

  rm(x)
  expect_false(exists("x"))

  expect_message(MakeObj(x, { x <- 1:10 }), regexp = "loading")
  expect_identical(x, y)

  # Cleanup
  expect_true(file.remove(f))
  rm(x)
  expect_false(exists("x"))

})
