# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# FUNCTIONS ####################################################################

# =============================================================================.
# Make workspaces for tests
# -----------------------------------------------------------------------------.
something_to_test <- function() {
  resetLittleThumb(ask = F)
  LTE <- .lte_env.()
  cfg <- LTE$config

  path <- make_path(cfg, "USRDIR")
  path1 <- make_path(path, "TestWorkspace1")
  path2 <- make_path(path, "TestWorkspace2")
  clear_path(path1)
  clear_path(path2)

  # define_workspace("WS1", path1)
  # create_workspace("WS1")
  #
  # define_workspace("WS2", path2)
  # create_workspace("WS2")

  list(cfg = cfg, path1 = path1, path2 = path2)
}

# DEFINITION ===================================================================
context(.ttmsg.("LT_Workspace", "definition"))

# + new ------------------------------------------------------------------------
test_that("new", {

})

# > Workspaces =================================================================
context(.ttmsg.("> Workspaces", "functions"))

# + define_workspace -----------------------------------------------------------
test_that("define_workspace", {

  # Initializations
  tst <- something_to_test()

  # Tests
  expect_error(define_workspace("WS1"))
  expect_error(define_workspace("WS2"))
  expect_error(define_workspace(path = tst$path1))
  expect_error(define_workspace(path = tst$path2))

  define_workspace("WS1", tst$path1)
  expect_error(define_workspace("WS1", tst$path1))
  expect_error(define_workspace("WS1", tst$path2))
  expect_error(define_workspace("WS2", tst$path1))

  define_workspace("WS2", tst$path2)
  expect_identical(LTE$workspaces$name, c("WS1", "WS2"))
  expect_identical(LTE$workspaces$is_created, c(F, F))
})

# + list_workspaces ------------------------------------------------------------
test_that("list_workspaces", {

  # Initializations
  tst <- something_to_test()
  define_workspace("WS1", tst$path1)
  define_workspace("WS2", tst$path2)

  # Tests
  expect_identical(list_workspaces(detailed = T)$name, c("WS1", "WS2"))
  expect_identical(list_workspaces(), c("WS1", "WS2"))
  expect_identical(list_workspaces(name == "WS1"), "WS1")
  expect_identical(list_workspaces(name == "WS2"), "WS2")
})

# + create_workspace -----------------------------------------------------------
test_that("create_workspace", {

  # Initializations
  tst <- something_to_test()
  lst <- list_paths(tst$cfg, is_dir == T  & level == "workspace")

  # Tests
  define_workspace("WS1", tst$path1)
  create_workspace("WS1")

  define_workspace("WS2", tst$path2)
  create_workspace("WS2")

  expect_error(create_workspace("WS1", tst$path1))
  expect_error(create_workspace("WS2", tst$path2))

  expect_identical(LTE$workspaces$name, c("WS1", "WS2"))
  expect_identical(LTE$workspaces$is_created, c(T, T))

  flp <- make_paths(tst$cfg, lst, root = tst$path1)
  expect_true(all(file.exists(flp)))

  flp <- make_paths(tst$cfg, lst, root = tst$path2)
  expect_true(all(file.exists(flp)))
})

# + open & close -----------------------------------------------------------
test_that("open & close", {

  # Initializations
  tst <- something_to_test()
  define_workspace("WS1", tst$path1)
  create_workspace("WS1")

  define_workspace("WS2", tst$path2)
  create_workspace("WS2")

  # Tests
  open_workspace("WS1")
  expect_identical(LTE$workspaces$is_opened, c(T, F))

})
