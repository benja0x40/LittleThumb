# > MakePath ==================================================================
context("MakePath")

# + Basic ----------------------------------------------------------------------
test_that("Basic", {

  expect_error(MakePath(), regexp = "empty")

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

  LittleThumb(rootpath = "Somewhere", relative = FALSE)

  expect_error(MakePath(), regexp = "empty")
  expect_error(MakePath(ext = ".z"), regexp = "empty")

  expect_identical("x/y.z", MakePath("x/", "/y", ext = ".z"))

  LittleThumb(rootpath = "Somewhere", relative = TRUE)

  expect_error(MakePath(), regexp = "empty")
  expect_error(MakePath(ext = ".z"), regexp = "empty")

  expect_identical("Somewhere/x/y.z", MakePath("x/", "/y", ext = ".z"))

  LittleThumb(rootpath = "", relative = TRUE)

  expect_identical("x/y.z", MakePath("x/", "/y", ext = ".z"))

  do.call(LittleThumb, cfg) # Restore default values
  expect_identical(cfg, LittleThumb())

})

# + PathToRDS ------------------------------------------------------------------
test_that("PathToRDS", {

  name <- "obj"
  ext  <- ".z"
  rel  <- FALSE

  expect_identical(PathToRDS(name,        NULL, ext, rel), "obj.z")
  expect_identical(PathToRDS(name,          "", ext, rel), "obj.z")
  expect_identical(PathToRDS(name,       "x/y", ext, rel), "x/y/obj.z")
  expect_identical(PathToRDS(name, c("x", "y"), ext, rel), "x/y/obj.z")

  path <- c(xxx = "x", yyy = "y", zzz = "z")
  expect_identical(PathToRDS(name, path, ext, rel), "obj.z")
  expect_identical(PathToRDS(name, as.list(path), ext, rel), "obj.z")

  names(path)[2] <- "obj"
  expect_identical(PathToRDS(name, path, ext, rel), "y/obj.z")
  expect_identical(PathToRDS(name, as.list(path), ext, rel), "y/obj.z")

})

