# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# CFG ==========================================================================
context(.ttmsg.("> SelfInstallation", "config"))

# + .usr_dir. ------------------------------------------------------------------
test_that(".usr_dir.", {

  options(LittleThumb.testing = T)
  a <- .usr_dir.()

  options(LittleThumb.testing = F)
  b <- .usr_dir.()

  expect_true(grepl("Test$", a, perl = T))
  expect_false(grepl("Test$", b, perl = T))

  options(LittleThumb.testing = T)
})

# + .pkg_cfg. ------------------------------------------------------------------
test_that(".pkg_cfg.", {

  options(LittleThumb.testing = T)
  expect_identical(.pkg_cfg.(), "TestConfig")

  options(LittleThumb.testing = F)
  expect_identical(.pkg_cfg.(), "DefaultConfig")
  expect_identical(.pkg_cfg.(), .pkg_cfg.())

  options(LittleThumb.testing = T)
})

# + .pkg_config_file. ----------------------------------------------------------
test_that(".pkg_config_file.", {

  options(LittleThumb.testing = T)
  flp <- .pkg_config_file.()
  expect_true(file.exists(dirname(flp)))
  expect_true(grepl("TestConfig$", flp, perl = T))

  options(LittleThumb.testing = F)
  flp <- .pkg_config_file.()
  expect_true(file.exists(dirname(flp)))
  expect_true(grepl("DefaultConfig$", flp, perl = T))

  options(LittleThumb.testing = T)
})

# + .make_pkg_config. ----------------------------------------------------------
test_that(".make_pkg_config.", {

  options(LittleThumb.testing = T)

  flp <- .make_pkg_config.(save = F)
  # suppressWarnings(file.remove(flp))
  expect_true(grepl("TestConfig$", flp, perl = T))

  flp <- .make_pkg_config.(save = T)
  expect_true(file.exists(flp))

  cfg <- lt_load(.pkg_config_file.())
  expect_is(cfg, "LT_Config")

  options(LittleThumb.testing = T)
})

