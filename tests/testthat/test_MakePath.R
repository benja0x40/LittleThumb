# > MakePath ==================================================================
context("MakePath")

# + MakePath -------------------------------------------------------------------
test_that("MakePath", {

  expect_error(MakePath(), regexp = "empty")
  expect_error(MakePath(ext = ".z"), regexp = "empty")

  expect_identical("x/y/z", MakePath("x", "y", "z"))
  expect_identical("x/y/z", MakePath("x/", "y/", "z"))
  expect_identical("x/y/z", MakePath("x", "/y", "/z"))

  expect_identical("x/y/z", MkPath("x/", "/y/", "/z"))
  expect_identical("x/y/z", MkPath("x//y", "/z"))
  expect_identical("x/y/z", MkPath("x", "y//z"))

  expect_identical("/x/y/z", MakePath("//x", "y", "z"))
  expect_identical("x/y/z/", MakePath("x", "y", "z//"))

  expect_identical("x/y.z", MakePath("x", "y", ext = ".z"))

})

# + PathToRDS ------------------------------------------------------------------
test_that("PathToRDS", {

  cfg <- LittleThumb() # Global options

  name <- "obj"
  ext  <- ".z"
  rel  <- FALSE

  expect_identical(PathToRDS(name,        NULL), "obj.rds")
  expect_identical(PathToRDS(name,          ""), "obj.rds")
  expect_identical(PathToRDS(name,       "x/y"), "x/y/obj.rds")
  expect_identical(PathToRDS(name, c("x", "y")), "x/y/obj.rds")

  path <- c(xxx = "x", yyy = "y", zzz = "z")
  expect_identical(PathToRDS(name, path), "obj.rds")
  expect_identical(PathToRDS(name, as.list(path)), "obj.rds")

  names(path)[2] <- "obj"
  expect_identical(PathToRDS(name, path), "y/obj.rds")
  expect_identical(PathToRDS(name, as.list(path)), "y/obj.rds")

  LittleThumb(rootpath = "Somewhere", relative = FALSE)
  expect_identical(PathToRDS(name,    ""), "obj.rds")
  expect_identical(PathToRDS(name, "x/y"), "x/y/obj.rds")

  LittleThumb(rootpath = "Somewhere", relative = TRUE)
  expect_identical(PathToRDS(name,    ""), "Somewhere/obj.rds")
  expect_identical(PathToRDS(name, "x/y"), "Somewhere/x/y/obj.rds")

  RegisterObject("a")
  RegisterObject("b")
  RegisterObject(name)
  SetParent(name, "b")
  SetParent("b", "a")

  emb <- FALSE

  rel <- FALSE
  expect_identical(PathToRDS(name,    "", rel, emb), "obj.rds")
  expect_identical(PathToRDS(name, "x/y", rel, emb), "x/y/obj.rds")

  rel <- TRUE
  expect_identical(PathToRDS(name,    "", rel, emb), "Somewhere/obj.rds")
  expect_identical(PathToRDS(name, "x/y", rel, emb), "Somewhere/x/y/obj.rds")

  emb <- TRUE

  rel <- FALSE
  expect_identical(
    PathToRDS(name,    "", rel, emb),
    "_components_/a/_components_/b/obj.rds"
  )
  expect_identical(
    PathToRDS(name, "x/y", rel, emb),
    "x/y/_components_/a/_components_/b/obj.rds"
  )

  rel <- TRUE
  expect_identical(
    PathToRDS(name,    "", rel, emb),
    "Somewhere/_components_/a/_components_/b/obj.rds"
  )
  expect_identical(
    PathToRDS(name, "x/y", rel, emb),
    "Somewhere/x/y/_components_/a/_components_/b/obj.rds"
  )

  # Cleanup
  LittleThumb::ResetRegistry()
  LittleThumb::ResetOptions()
})

