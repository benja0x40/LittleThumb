# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# DEFINITION ===================================================================
context(.ttmsg.("LT_Config", "definition"))

# + new ------------------------------------------------------------------------
test_that("new", {

  r <- LT_Config()

  expect_s3_class(r, c("LT_Config", "LT_Data", "list"))
  expect_true(inherits(r, "LT_Config"))
  expect_true(inherits(r, "LT_Data"))
  expect_true(inherits(r, "list"))
})

# > FileSystem =================================================================
context(.ttmsg.("LT_Config", "> FileSystem"))

# + make_path ------------------------------------------------------------------
test_that("make_path", {

  cfg <- lt_load(.pkg_config_file.())

  r <- make_path(cfg, name = "USRDIR")
  expect_identical(r, "~/.LittleThumbTest")

  r <- make_path(cfg, name = "CFGDIR")
  expect_identical(r, "_LittleThumb_/config")

  r <- make_path(cfg, name = "CFGDIR", root = "MyWorkspace")
  expect_identical(r, "MyWorkspace/_LittleThumb_/config")
})

# + list_paths -----------------------------------------------------------------
test_that("list_paths", {

  cfg <- lt_load(.pkg_config_file.())

  lst <- list_paths(cfg)
  expect_identical(lst, with(cfg$TD, label))

  lst <- list_paths(cfg, is_dir == T)
  expect_identical(lst, with(cfg$TD, label[is_dir == T]))

  lst <- list_paths(cfg, level == "user")
  expect_identical(lst, with(cfg$TD, label[level == "user"]))

  lst <- list_paths(cfg)
  lst <- make_paths(cfg, name = lst)

})

