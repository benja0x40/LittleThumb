# > LittleThumb ================================================================
context("LittleThumb")

# + DefaultOptions -------------------------------------------------------------
test_that("DefaultOptions", {

  expect_identical(LittleThumb::DefaultOptions(), LittleThumb())

})

# + Arbitrary options ----------------------------------------------------------
test_that("Arbitrary options", {

  cfg <- LittleThumb()
  opt <- names(cfg)

  x <- "SomeValue"
  a <- list()
  for(k in opt) {
    a[[k]] <- x
    do.call(LittleThumb, a)
    expect_identical(LittleThumb()[[k]], x, info = k)
    a[[k]] <- NULL
  }

  do.call(LittleThumb, cfg) # Restore default values
  expect_identical(cfg, LittleThumb())

})

# + Remove & Reset -------------------------------------------------------------
test_that("Remove & Reset", {

  LittleThumb::RemoveOptions()
  expect_error(LittleThumb())
  LittleThumb::ResetOptions()
  expect_identical(LittleThumb::DefaultOptions(), LittleThumb())

})

# + StatusMessage --------------------------------------------------------------
test_that("StatusMessage", {

  expect_message(LittleThumb::StatusMessage(""), "LittleThumb")

})



