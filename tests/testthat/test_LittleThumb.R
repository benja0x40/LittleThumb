# > LittleThumb ================================================================
context("LittleThumb")

cfg <- LittleThumb::DefaultOptions() # LittleThumb options

# + LogicalArg -----------------------------------------------------------------
test_that("LogicalArg", {

  expect_true(LogicalArg("x", T) == T)
  expect_true(LogicalArg("x", F) == F)

  expect_true(LogicalArg("x", "x") == T)
  expect_true(LogicalArg("x", "y") == F)
  expect_true(LogicalArg("x", c("x", "y")) == T)
  expect_true(LogicalArg("x", c("y", "z")) == F)

  expect_true(LogicalArg("x", c(x = T)) == T)
  expect_true(LogicalArg("x", c(x = F)) == F)
  expect_true(LogicalArg("x", c(x = T, y = F)) == T)
  expect_true(LogicalArg("x", c(x = F, y = T)) == F)
  expect_true(LogicalArg("x", c(y = T, z = T)) == F)

  expect_true(LogicalArg("x", list(x = T)) == T)
  expect_true(LogicalArg("x", list(x = F)) == F)
  expect_true(LogicalArg("x", list(x = T, y = F)) == T)
  expect_true(LogicalArg("x", list(x = F, y = T)) == F)
  expect_true(LogicalArg("x", list(y = T, z = T)) == F)

})

# + Default options ------------------------------------------------------------
test_that("Default options", {

  expect_identical(LittleThumb::DefaultOptions(), LittleThumb())

})

# + Arbitrary options ----------------------------------------------------------
test_that("Arbitrary options", {

  expect_identical(LittleThumb::DefaultOptions(), LittleThumb())

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

