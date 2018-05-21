# > LittleThumb ================================================================
context("LittleThumb")

cfg <- lt_cfg() # LittleThumb options
lst <- c(
  "path", "root", "extension",
  "makedir", "rebuild", "overload",
  "environment"
)

# + Default options ------------------------------------------------------------
test_that("Default options", {

  expect_identical(names(cfg), lst)

  # Path generation
  expect_identical(cfg$path, "")
  expect_identical(cfg$root, T)
  expect_identical(cfg$extension, ".rds")

  # Behavior
  expect_identical(cfg$makedir, T)
  expect_identical(cfg$rebuild, F)
  expect_identical(cfg$overload, F)

  # Evaluation
  expect_identical(cfg$environment, NA)

})

# + Arbitrary options ----------------------------------------------------------
test_that("Arbitrary options", {

  expect_identical(cfg, LittleThumb())

  x <- "SomeValue"
  a <- list()
  for(k in lst) {
    a[[k]] <- x
    do.call(LittleThumb, a)
    expect_identical(lt_cfg()[[k]], x, info = k)
    a[[k]] <- NULL
  }

  do.call(LittleThumb, cfg) # Restore default values
  expect_identical(cfg, LittleThumb())

})

