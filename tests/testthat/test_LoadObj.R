# > LoadObj ====================================================================
context("LoadObj")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  cfg <- LittleThumb() # Global options

  # LittleThumb(envir = sys.frame(sys.nframe()))

  f <- MakePath("x", ext = cfg$extension)
  x <- y <- 1:10

  expect_error(LoadObj(x))
  saveRDS(x, f)

  rm(x)
  expect_false(exists("x"))

  expect_message(LoadObj(x), regexp = "load")
  expect_true(exists("x"))
  expect_identical(x, y)

  expect_message(LoadObj(x), regexp = "bypass")
  expect_identical(x, y)

  x <- 0
  expect_message(LoadObj(x), regexp = "bypass")
  expect_identical(x, 0)

  expect_message(LoadObj(x, overload = T), regexp = "overload")
  expect_identical(x, y)

  # Cleanup
  expect_true(file.remove(f))
  rm(x)
  expect_false(exists("x"))

  do.call(LittleThumb, cfg) # Restore default values
  expect_identical(cfg, LittleThumb())
})
