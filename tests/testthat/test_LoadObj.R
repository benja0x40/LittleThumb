# > LoadObj ====================================================================
context("LoadObj")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  cfg <- lt_cfg() # LittleThumb options
  # LittleThumb(environment = sys.frame(sys.nframe()))

  f <- MakePath("x", ext = cfg$extension)
  x <- y <- 1:10

  expect_error(LoadObj(x))
  SaveObj(x)

  rm(x)
  expect_false(exists("x"))

  expect_message(LoadObj(x), regexp = "[loading]")
  expect_true(exists("x"))
  expect_identical(x, y)

  expect_message(LoadObj(x), regexp = "[passing]")
  expect_identical(x, y)

  x <- 0
  expect_message(LoadObj(x), regexp = "[passing]")
  expect_identical(x, 0)

  expect_message(LoadObj(x, overload = T), regexp = "[overloading]")
  expect_identical(x, y)

  # Cleanup
  expect_true(file.remove(f))
  rm(x)
  expect_false(exists("x"))

  do.call(LittleThumb, cfg) # Restore default values
  expect_identical(cfg, LittleThumb())
})

# + Advanced -------------------------------------------------------------------
test_that("Advanced", {

  cfg <- lt_cfg() # LittleThumb options


  do.call(LittleThumb, cfg) # Restore default values

})

