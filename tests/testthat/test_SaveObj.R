# > SaveObj ====================================================================
context("SaveObj")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  cfg <- LittleThumb() # Global options

  # LittleThumb(envir = sys.frame(sys.nframe()))

  f <- MakePath("x", ext = cfg$extension)
  x <- 1:10

  expect_false(file.exists(f))
  expect_message(SaveObj(x), regexp = "[saving]")
  expect_true(file.exists(f))
  expect_identical(readRDS(f), x)

  x <- 10:1
  expect_message(SaveObj(x), regexp = "[overwriting]")
  expect_identical(readRDS(f), x)

  # Cleanup
  expect_true(file.remove(f))

  do.call(LittleThumb, cfg) # Restore default values
  expect_identical(cfg, LittleThumb())
})

# + Advanced -------------------------------------------------------------------
test_that("Advanced", {

})

