# > DeleteObj ==================================================================
context("DeleteObj")

# + Basics ---------------------------------------------------------------------
test_that("Basics", {

  expect_error(DeleteObj(), regexp = "insufficient")
  expect_warning(DeleteObj(x))
  expect_message(suppressWarnings(DeleteObj(x)), regexp = "not found")

  fx <- PathToRDS("x")
  fy <- PathToRDS("y")
  x <- y <- 1:10
  saveRDS(x, fx)
  saveRDS(y, fy)

  expect_true(file.exists(fx))
  expect_true(file.exists(fy))
  expect_true(exists("x"))
  expect_true(exists("y"))

  DeleteObj(x)
  expect_false(file.exists(fx))
  expect_false(exists("x"))

  DeleteObj(y, remove = F)
  expect_false(file.exists(fy))
  expect_true(exists("y"))

  # Cleanup
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})
