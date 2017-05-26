# PKG OPTIONS ##################################################################

options(LittleThumb.testing = T)

# FUNCTIONS ####################################################################

# =============================================================================.
# Make workspaces for tests
# -----------------------------------------------------------------------------.
make_test_data <- function(path, tag, n = 10, m = 100) {

  dir.create(path, showWarnings = F, recursive = T)

  ann  <- data.frame(
    name = paste0(tag, "_", LETTERS[1:n]),
    number = 0,
    stringsAsFactors = F
  )
  ann$file <- paste0(ann$name, ext = ".txt")

  x <- vector("list", n)
  for(i in 1:n) {

    x[[i]] <-   data.frame(
      position = paste0(LETTERS[i], str_pad(1:m, width = 4, pad = "0")),
      value = rpois(m, 2)
    )
    flp <- make_path(path, ann$file[i])
    write.table(
      x[[i]], flp, quote = F, row.names = F, col.names = T, sep = "\t"
    )
    ann$number[i] <- sum(x[[i]]$value)
  }

  flp <- make_path(path, "Annotations.txt")
  write.table(ann, flp, quote = F, row.names = F, col.names = T, sep = "\t")
}

# =============================================================================.
# Make workspaces for tests
# -----------------------------------------------------------------------------.
something_to_test <- function(path = NULL) {

  resetLittleThumb(ask = F)
  LTE <- lt_env()
  cfg <- LTE$config

  if(is.null(path)) path <- make_path(cfg, "USRDIR")

  path1 <- make_path(path, "TestWorkspace1")
  path2 <- make_path(path, "TestWorkspace2")
  dataX <- make_path(path, "TestDataX")
  dataY <- make_path(path, "TestDataY")
  dataZ <- make_path(path, "TestDataZ")
  tst <- list(
    cfg = cfg,
    path1 = path1, path2 = path2,
    dataX = dataX, dataY = dataY, dataZ = dataZ,
    cleanup = c(path1, path2, dataX, dataY, dataZ)

  )
  make_test_data(dataX, tag = "TDX", n = 2, m = 100)
  make_test_data(dataY, tag = "TDY", n = 4, m = 50)
  make_test_data(dataZ, tag = "TDZ", n = 8, m = 25)

  tst
}

# =============================================================================.
# Make workspaces for tests
# -----------------------------------------------------------------------------.
cleanup <- function(tst) {
  resetLittleThumb(ask = F)
  suppressWarnings(sapply(tst$cleanup, clear_path))
}

# TESTS ########################################################################

# > Workspaces =================================================================
context(.ttmsg.("LT_Workspace", "functions"))

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

  # Cleanup
  cleanup(tst)
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
  expect_identical(list_workspaces(F, name == "WS1"), "WS1")
  expect_identical(list_workspaces(F, name == "WS2"), "WS2")

  # Cleanup
  cleanup(tst)
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

  # Cleanup
  cleanup(tst)
})

# + open & close ---------------------------------------------------------------
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
  expect_true("WS1" %in% ls(pos = globalenv()))
  expect_is(get("WS1", pos = globalenv()), "LT_Workspace")
  expect_is(get("WS1", pos = globalenv()), "environment")

  open_workspace("WS2")
  expect_identical(LTE$workspaces$is_opened, c(T, T))
  expect_true("WS2" %in% ls(pos = globalenv()))
  expect_is(get("WS2", pos = globalenv()), "LT_Workspace")
  expect_is(get("WS2", pos = globalenv()), "environment")

  close_workspace("WS1")
  expect_identical(LTE$workspaces$is_opened, c(F, T))
  expect_false("WS1" %in% ls(pos = globalenv()))

  close_workspace("WS2")
  expect_identical(LTE$workspaces$is_opened, c(F, F))
  expect_false("WS2" %in% ls(pos = globalenv()))

  # Cleanup
  cleanup(tst)
})

# > FileSystem =================================================================
context(.ttmsg.("LT_Workspace", "> FileSystem"))

# + self_path ------------------------------------------------------------------
test_that("self_path", {

  # Initializations
  tst <- something_to_test()
  define_workspace("WS1", tst$path1)
  create_workspace("WS1")
  open_workspace("WS1")

  define_workspace("WS2", tst$path2)
  create_workspace("WS2")
  open_workspace("WS2")

  # Tests
  expect_identical(
    self_path(get("WS1", pos = globalenv())), normalizePath(tst$path1)
  )
  expect_identical(
    self_path(get("WS2", pos = globalenv())), normalizePath(tst$path2)
  )

  # Cleanup
  cleanup(tst)
})


