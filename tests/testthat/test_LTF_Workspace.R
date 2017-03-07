# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# Workspace ====================================================================
context(.ttmsg.("> Workspaces", "functions"))

# + define_workspace -----------------------------------------------------------
test_that("define_workspace", {

  # Initialization
  resetLittleThumb(ask = F)
  LTE <- .lte_env.()
  cfg <- LTE$config

  path <- make_path(cfg, "USRDIR")
  path1 <- make_path(path, "TestWorkspace1")
  path2 <- make_path(path, "TestWorkspace2")
  clear_path(path1)
  clear_path(path2)

  # Tests
  expect_error(define_workspace("test1"))
  expect_error(define_workspace("test2"))
  expect_error(define_workspace(path = path1))
  expect_error(define_workspace(path = path2))

  define_workspace("test1", path1)
  expect_error(define_workspace("test1", path1))
  expect_error(define_workspace("test1", path2))
  expect_error(define_workspace("test2", path1))

  define_workspace("test2", path2)

  expect_identical(LTE$workspaces$name, c("test1", "test2"))
  expect_identical(LTE$workspaces$is_created, c(F, F))

  clear_path(path1)
  clear_path(path2)
})

# + create_workspace -----------------------------------------------------------
test_that("create_workspace", {

  # Initialization
  resetLittleThumb(ask = F)
  LTE <- .lte_env.()
  cfg <- LTE$config

  lst <- list_paths(cfg, is_dir == T  & level == "workspace")

  path <- make_path(cfg, "USRDIR")
  path1 <- make_path(path, "TestWorkspace1")
  path2 <- make_path(path, "TestWorkspace2")
  clear_path(path1)
  clear_path(path2)

  # Tests
  define_workspace("test1", path1)
  create_workspace("test1")

  define_workspace("test2", path2)
  create_workspace("test2")

  expect_error(create_workspace("test1", path1))
  expect_error(create_workspace("test2", path2))

  expect_identical(LTE$workspaces$name, c("test1", "test2"))
  expect_identical(LTE$workspaces$is_created, c(T, T))

  flp <- make_paths(cfg, lst, root = path1)
  expect_true(all(file.exists(flp)))

  flp <- make_paths(cfg, lst, root = path2)
  expect_true(all(file.exists(flp)))

  # clear_path(path1)
  # clear_path(path2)
})

