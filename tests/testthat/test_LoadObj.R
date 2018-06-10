# > LoadObj ====================================================================
context("LoadObj")

# + Basics ---------------------------------------------------------------------
test_that("Basics", {

  f <- PathToRDS("x")
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

  expect_message(LoadObj(x, reload = TRUE), regexp = "reload")
  expect_identical(x, y)

  # Cleanup
  expect_true(file.remove(f))
  rm(x)
  expect_false(exists("x"))

  # Cleanup
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})
