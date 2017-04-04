# FUNCTIONS ####################################################################
#
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
  LTE <- .lte_env.()
  cfg <- LTE$config

  if(is.null(path)) path <- make_path(cfg, "USRDIR")

  make_test_data(make_path(path, "TestDataX"), tag = "TDX", n = 2, m = 100)
  make_test_data(make_path(path, "TestDataY"), tag = "TDY", n = 4, m = 50)
  make_test_data(make_path(path, "TestDataZ"), tag = "TDZ", n = 8, m = 25)

  path1 <- make_path(path, "TestWorkspace1")
  path2 <- make_path(path, "TestWorkspace2")
  clear_path(path1)
  clear_path(path2)

  define_workspace("WS1", path1)
  create_workspace("WS1")

  define_workspace("WS2", path2)
  create_workspace("WS2")

  list(cfg = cfg, path1 = path1, path2 = path2)
}

# TESTS ########################################################################

# =============================================================================.
root_path <- "./DEVTESTS"
root_path <- "/media/SSD512GB/TESTS"

something_to_test(path = root_path)

list_workspaces(detailed = T)

open_workspace("WS1")
create_dataset(
  "WS1", name = "TDX",
  source_path = make_path(root_path, "TestDataX"), pattern = "TD"
)
create_dataset(
  "WS1", name = "TDY",
  source_path = make_path(root_path, "TestDataY"), pattern = "TD"
)
create_dataset(
  "WS1", name = "TDZ",
  source_path = make_path(root_path, "TestDataZ"), pattern = "TD"
)

open_workspace("WS2")
create_dataset(
  "WS2", path = "TDX",
  source_path = make_path(root_path, "TestDataX"), pattern = "TD",
  annotations = make_path(root_path, "TestDataX", "Annotations.txt"),
  id_column = "name", file_columns = "file"
)

create_dataset(
  "WS2", path = "TDY",
  source_path = make_path(root_path, "TestDataY"), pattern = "TD",
  annotations = make_path(root_path, "TestDataY", "Annotations.txt"),
  id_column = "name", file_columns = "file"
)

create_dataset(
  "WS2", path = "TDZ",
  source_path = make_path(root_path, "TestDataZ"),
  annotations = make_path(root_path, "TestDataZ", "Annotations.txt"),
  id_column = "name", file_columns = "file"
)

closeLittleThumb()

# =============================================================================.
openLittleThumb()
list_workspaces(detailed = T)
list_datasets(detailed = T)

open_workspace("WS1")
open_workspace("WS2")

load_data(workspace = "WS2", stringsAsFactors = F)
close_data(workspace = "WS2")

# =============================================================================.
resetLittleThumb(ask = F)
rm(list = ls())
openLittleThumb()

# =============================================================================.
root_path <- "/data/ANALYSIS_LabProjects/Project_ChIP-seq_Normalization/FMO_SpikeIN_20160811"
define_workspace("WS1", "~/LT_DEV_TESTS")
create_workspace("WS1")
open_workspace("WS1")

ann <- read.delim(
  make_path(root_path, "SampleAnnotations.txt"), stringsAsFactors = F
)
lst <- paste0(root_path, "/ALL_RDATA/ALN/", ann$Name, ".rdata")
checklist(file.exists(lst), lst)

# stop("Here")

create_dataset(
  "WS1", path = "MAPPED_READS", files = lst,
  annotations = ann,
  id_column = "Name", file_columns = "file"
)

load_data("WS1", reader = readRDS)

filter_reads <- function(
  workspace = NULL, dataset = NULL, element = NULL, ...
) {

  fr <- function(wks, dts, fun, ...) {
    env <- globalenv()
    env[[wks]][[dts$name]] <- lapply(
      env[[wks]][[dts$name]], function(x) x[with(x) < 76]
    )
  }
  apply2dataset(executor = cd, fun = NULL, workspace, dataset, element, ...)
}


