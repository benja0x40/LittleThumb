# > LittleThumb ================================================================
context("LittleThumb")

# + Default options ------------------------------------------------------------
test_that("Default options", {

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
