# > SaveObj ====================================================================
context("SaveObj")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  cfg <- LittleThumb() # Global options

  # LittleThumb(envir = sys.frame(sys.nframe()))

  f <- MakePath("x", ext = cfg$extension)
  x <- 1:10

  expect_error(SaveObj(z))

  expect_false(file.exists(f))
  expect_message(SaveObj(x), regexp = "save")
  expect_true(file.exists(f))
  expect_identical(readRDS(f), x)

  x <- 10:1
  expect_message(SaveObj(x), regexp = "overwrite")
  expect_identical(readRDS(f), x)

  LittleThumb(rootpath = "./_LT_RDATA_")

  expect_message(SaveObj(x, relative = F), regexp = "overwrite")
  expect_true(file.remove(f))

  f <- MakePath("x", ext = cfg$extension)

  expect_message(SaveObj(x, relative = T), regexp = "create")
  expect_true(file.exists(f))
  expect_identical(readRDS(f), x)
  expect_true(file.remove(f))

  # Cleanup
  unlink("./_LT_RDATA_", recursive = T)

  do.call(LittleThumb, cfg) # Restore default values
  expect_identical(cfg, LittleThumb())
})
