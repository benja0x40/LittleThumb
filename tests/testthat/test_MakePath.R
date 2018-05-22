# > MakePath ==================================================================
context("MakePath")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  expect_identical(MakePath(), "")

  expect_identical("x/y/z", MakePath("x", "y", "z"))
  expect_identical("x/y/z", MakePath("x/", "y/", "z"))
  expect_identical("x/y/z", MakePath("x", "/y", "/z"))
  expect_identical("x/y/z", MakePath("x/", "/y/", "/z"))
  expect_identical("x/y/z", MakePath("x//y", "/z"))
  expect_identical("x/y/z", MakePath("x", "y//z"))

  expect_identical("/x/y/z", MakePath("//x", "y", "z"))
  expect_identical("x/y/z/", MakePath("x", "y", "z//"))

  expect_identical("x/y.z", MakePath("x", "y", ext = ".z"))

})

# + Advanced -------------------------------------------------------------------
test_that("Advanced", {

  cfg <- LittleThumb() # Global options

  LittleThumb(path = "Somewhere", relative = T)

  expect_identical(MakePath(), "Somewhere")

  expect_identical("Somewhere/x/y/z", MakePath("x/", "/y/", "/z"))
  expect_identical("Somewhere/x/y/z", MakePath("/x", "y", "z"))
  expect_identical("Somewhere/x/y.z", MakePath("x", "y", ext = ".z"))

  LittleThumb(path = "Somewhere", relative = F)

  expect_identical(MakePath(), "Somewhere")

  expect_identical("x/y/z", MakePath("x/", "/y/", "/z"))
  expect_identical("/x/y/z", MakePath("/x", "y", "z"))
  expect_identical("x/y.z", MakePath("x", "y", ext = ".z"))

  do.call(LittleThumb, cfg) # Restore default values
  expect_identical(cfg, LittleThumb())

})

