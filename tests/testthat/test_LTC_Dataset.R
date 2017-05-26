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

  define_workspace("WS1", path1)
  create_workspace("WS1")

  define_workspace("WS2", path2)
  create_workspace("WS2")

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

# > Datasets ===================================================================
context(.ttmsg.("LT_Dataset", "functions"))

# + create_dataset -------------------------------------------------------------
test_that("create_dataset", {

  # Initializations
  tst <- something_to_test()
  open_workspace("WS1")
  open_workspace("WS2")

  create_dataset("WS1", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS1", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS1", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  create_dataset(
    "WS2", path = "TDX",
    source_path = tst$dataX, pattern = "TD",
    annotations = make_path(tst$dataX, "Annotations.txt"),
    id_column = "name", file_columns = "file"
  )

  create_dataset(
    "WS2", path = "TDY",
    source_path = tst$dataY, pattern = "TD",
    annotations = make_path(tst$dataY, "Annotations.txt"),
    id_column = "name", file_columns = "file"
  )

  create_dataset(
    "WS2", path = "TDZ",
    source_path = tst$dataZ,
    annotations = make_path(tst$dataZ, "Annotations.txt"),
    id_column = "name", file_columns = "file"
  )

  WS1 <- globalenv()[["WS1"]]
  WS2 <- globalenv()[["WS2"]]

  # Tests
  expect_identical(LTE$datasets$workspace, rep(c("WS1", "WS2"), each = 3))

  expect_identical(LTE$datasets$name, rep(c("TDX", "TDY", "TDZ"), 2))
  expect_identical(LTE$datasets$path, rep(c("TDX", "TDY", "TDZ"), 2))
  expect_equal(LTE$datasets$items, rep(c(2, 4, 8), 2))

  expect_identical(names(WS1@datasets), LTE$datasets$id[1:3])
  expect_identical(names(WS2@datasets), LTE$datasets$id[4:6])
  # Cleanup
  cleanup(tst)
})

# + list_datasets --------------------------------------------------------------
test_that("list_datasets", {

  # Initializations
  tst <- something_to_test()
  open_workspace("WS1")
  open_workspace("WS2")

  create_dataset("WS1", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS1", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS1", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  create_dataset("WS2", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS2", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS2", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  # Tests
  expect_identical(list_datasets(), rep(c("TDX", "TDY", "TDZ"), 2))
  expect_identical(list_datasets(workspace = "WS1"), c("TDX", "TDY", "TDZ"))
  expect_identical(list_datasets(workspace = "WS2"), c("TDX", "TDY", "TDZ"))
  expect_identical(
    list_datasets(workspace = "WS1", detailed = T), LTE$datasets[1:3, ]
  )
  expect_identical(
    list_datasets(workspace = "WS2", detailed = T), LTE$datasets[4:6, ]
  )

  # Cleanup
  cleanup(tst)
})

# + which_dataset --------------------------------------------------------------
test_that("which_dataset", {

  # Initializations
  tst <- something_to_test()
  open_workspace("WS1")
  open_workspace("WS2")

  create_dataset("WS1", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS1", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS1", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  create_dataset("WS2", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS2", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS2", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  WS1 <- globalenv()[["WS1"]]
  WS2 <- globalenv()[["WS2"]]

  # Tests
  idx <- which_dataset()
  expect_identical(idx, integer(0))

  idx <- which_dataset(names(WS1@datasets))
  expect_identical(idx, 1:3)
  idx <- which_dataset(names(WS2@datasets))
  expect_identical(idx, 4:6)

  idx <- which_dataset(workspace = "WS1")
  expect_identical(idx, 1:3)
  idx <- which_dataset(workspace = "WS2")
  expect_identical(idx, 4:6)

  idx <- which_dataset(workspace = "WS1", name = c("TDX", "TDY"))
  expect_identical(idx, 1:2)

  idx <- which_dataset(name = "TDX")
  expect_equal(idx, c(1, 4))
  idx <- which_dataset(name = "TDY")
  expect_equal(idx, c(2, 5))
  idx <- which_dataset(name = "TDZ")
  expect_equal(idx, c(3, 6))

  idx <- which_dataset(names(WS1@datasets), name = "TDY")
  expect_equal(idx, 2)
  idx <- which_dataset(names(WS2@datasets), name = "TDY")
  expect_equal(idx, 5)

  idx <- which_dataset(workspace = "WS1", name = "TDY")
  expect_equal(idx, 2)
  idx <- which_dataset(workspace = "WS2", name = "TDY")
  expect_equal(idx, 5)

  # Cleanup
  cleanup(tst)
})

# + dataset_object -------------------------------------------------------------
test_that("dataset_object", {

  # Initializations
  tst <- something_to_test()
  open_workspace("WS1")
  open_workspace("WS2")

  create_dataset("WS1", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS1", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS1", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  create_dataset("WS2", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS2", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS2", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  close_workspace("WS1")
  close_workspace("WS2")
  closeLittleThumb(ask = F)

  # Tests
  expect_null(dataset_object())
  expect_error(
    dataset_object(workspace = "WS1"),
    "query matches multiple datasets"
  )
  expect_error(
    dataset_object(name = "TDY"),
    "query matches multiple datasets"
  )
  expect_error(
    dataset_object(workspace = "WS1", name = "TDY"),
    "workspace is not opened: WS1"
  )

  open_workspace("WS1")
  open_workspace("WS2")

  WS1 <- globalenv()[["WS1"]]
  WS2 <- globalenv()[["WS2"]]

  expect_identical(
    dataset_object(workspace = "WS1", name = "TDX"),
    WS1@datasets[[1]]
  )
  expect_identical(
    dataset_object(workspace = "WS1", name = "TDZ"),
    WS1@datasets[[3]]
  )
  expect_identical(
    dataset_object(workspace = "WS2", name = "TDX"),
    WS2@datasets[[1]]
  )
  expect_identical(
    dataset_object(workspace = "WS2", name = "TDZ"),
    WS2@datasets[[3]]
  )

  # Cleanup
  cleanup(tst)
})

# > FileSystem =================================================================
context(.ttmsg.("LT_Dataset", "> FileSystem"))

# + self_path ------------------------------------------------------------------
test_that("self_path", {

  # Initializations
  tst <- something_to_test()
  open_workspace("WS1")
  open_workspace("WS2")

  create_dataset("WS1", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS1", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS1", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  create_dataset("WS2", name = "TDX", source_path = tst$dataX, pattern = "TD")
  create_dataset("WS2", name = "TDY", source_path = tst$dataY, pattern = "TD")
  create_dataset("WS2", name = "TDZ", source_path = tst$dataZ, pattern = "TD")

  close_workspace("WS1")
  close_workspace("WS2")
  closeLittleThumb(ask = F)

  # Tests
  open_workspace("WS1")
  open_workspace("WS2")

  expect_identical(
    self_path(dataset_object(workspace = "WS1", name = "TDY")), "TDY"
  )
  expect_identical(
    self_path(dataset_object(workspace = "WS2", name = "TDY")), "TDY"
  )

  # Cleanup
  cleanup(tst)
})
