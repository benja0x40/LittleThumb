# > SaveObj ====================================================================
context("SaveObj")

# + Basics ---------------------------------------------------------------------
test_that("Basics", {

  f <- PathToRDS("x")
  x <- 1:10

  expect_error(SaveObj(), regexp = "insufficient")
  expect_error(SaveObj(z))

  expect_false(file.exists(f))
  expect_message(SaveObj(x), regexp = "save")
  expect_true(file.exists(f))
  expect_identical(readRDS(f), x)

  x <- 10:1
  expect_message(SaveObj(x), regexp = "update")
  expect_identical(readRDS(f), x)

  LittleThumb(rootpath = "AutoSaved")

  expect_message(SaveObj(x, relative = FALSE), regexp = "update")
  expect_true(file.remove(f))

  f <- PathToRDS("x")

  expect_message(SaveObj(x, relative = TRUE), regexp = "create")
  expect_true(file.exists(f))
  expect_identical(readRDS(f), x)
  expect_true(file.remove(f))

  a <- list(x = 1)
  RegisterObject("a")
  SaveObj(x, parent.name = "a")
  f <- PathToRDS("x")
  expect_true(file.exists(f))

  # Cleanup
  unlink("AutoSaved", recursive = TRUE)
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})
