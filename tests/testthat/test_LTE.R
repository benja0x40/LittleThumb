# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# LTE ==========================================================================
context(.ttmsg.("LTE.R", "environment"))

# + .lte_name. -----------------------------------------------------------------
test_that(".lte_name.", {

  options(LittleThumb.testing = T)
  expect_identical(.lte_name.(), "LTE")

  options(LittleThumb.testing = F)
  expect_identical(.lte_name.(), ".LTE.")

  options(LittleThumb.testing = T)
})

# + .lte_is_loaded. ------------------------------------------------------------
test_that(".lte_is_loaded.", {

  options(LittleThumb.testing = T)

  resetLittleThumb(ask = F)
  expect_false(.lte_is_loaded.())
  expect_error(.lte_is_loaded.(error = T))
})

# + openLittleThumb ------------------------------------------------------------
test_that("openLittleThumb", {

  options(LittleThumb.testing = T)

  resetLittleThumb(ask = F)
  openLittleThumb()

  expect_true(.lte_is_loaded.())

  LTE <- lt_env()
  cfg <- LTE$config

  expect_is(cfg, "LT_Config")
  expect_true(file.exists(.lte_path.(cfg)))
})
