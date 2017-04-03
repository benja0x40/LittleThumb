# INCLUDES #####################################################################

# Generic and regular functions
#' @include LTF_Basics.R
#' @include LTF_R_Objects.R
#' @include LTF_Registration.R
#' @include LTF_TextRepresentation.R
#' @include LTF_FileSystem.R

# Class definitions & implementations
#' @include LTC_TabularData.R
#' @include LTC_MetaData.R
#' @include LTC_Data.R
#' @include LTC_Workspace.R

# S3 DEFINTION #################################################################

# =============================================================================.
# Inherits from: LT_Data
# =============================================================================.
#' LT_Dataset
# -----------------------------------------------------------------------------.
#' @description
#'
#' @param ...
#'
#' @return
#' an object of class LT_Dataset
# -----------------------------------------------------------------------------.
LT_Dataset <- function(...) {

  obj <- LT_Data(...)
  obj <- add_class(obj, "LT_Dataset")
  obj
}
# -----------------------------------------------------------------------------.
is.LT_Dataset <- function(x) { inherits(x, "LT_Dataset") }
# -----------------------------------------------------------------------------.

# METHODS ######################################################################

# > FileSystem #################################################################

# =============================================================================.
# Path to dataset definition
# -----------------------------------------------------------------------------.
lt_path.LT_Dataset <- function(obj, workspace, config, path_label = "DTSDIR") {
  LTE <- .lte_env.()
  path <- with(LTE$workspaces, path[match(workspace, name)])
  path <- make_path(path, make_path(config, path_label), obj$name, ext = ".txt")
  path
}

# =============================================================================.
# Path to dataset folder
# -----------------------------------------------------------------------------.
self_path.LT_Dataset <- function(obj) {
  obj$MD$path
}

# =============================================================================.
# Path to data files
# -----------------------------------------------------------------------------.
elements_path.LT_Dataset <- function(obj, name = NULL) {
  kid <- obj$MD$keys$id
  kfl <- obj$MD$keys$files
  if(is.null(name)) idx <- rep(T, nrow(obj$TD))
  else idx <- match(name, obj$TD[[kid]])
  flp <- paste0(obj$MD$path, "/", obj$TD[[kfl]][idx])
  flp
}
# =============================================================================.
# Path to data files
# -----------------------------------------------------------------------------.
elements_name <- function(obj) {
  kid <- obj$MD$keys$id
  obj$TD[[kid]]
}

# > Dataset ####################################################################

# =============================================================================.
# Return registered dataset names
# -----------------------------------------------------------------------------.
which_dataset <- function(id = NULL, workspace = NULL, name = NULL) {

  LTE <- .lte_env.()

  # TODO: should be based on reg_idx in data.frame Registration interface
  idx <- integer()
  if(! is.null(id)) {
    idx <- match(id, LTE$datasets$id)
  } else {
    chk <- ! any(is.null(workspace), is.null(name))
    checklist(chk, lst = "", msg = "workspace and name required")
    idx <- base::intersect(
      which(LTE$datasets$name %in% name),
      which(LTE$datasets$workspace %in% workspace)
    )
  }

  idx
}
# =============================================================================.
# Return registered dataset names
# -----------------------------------------------------------------------------.
list_datasets <- function(workspace = NULL, detailed = F, x = NULL) {

  LTE <- .lte_env.()

  if(is.null(workspace)) {
    workspace <- list_workspaces()
  } else {
    chk <- is_registered.data.frame(LTE$workspaces, x = workspace, error = T)
  }

  dts <- LTE$datasets
  if(nrow(dts) > 0) {
    dts <- dts[dts$workspace %in% workspace, ]
    if(! detailed) dts <- dts$name
  } else {
    if(! detailed) dts <- character()
  }

  dts
}

# =============================================================================.
# Create dataset definition and copy associated data inside a workspace folder
# -----------------------------------------------------------------------------.
# Use cases:
# - from a whole folder
# - from files within a folder
# - from a given list of files
# -----------------------------------------------------------------------------.
# Make minimal creation procedure (+ retrieve ideas below)
# and then complexify procedure to include annotations and key columns

create_dataset <- function(
  workspace, source_path = NULL, files = NULL, pattern = NULL,
  annotations = NULL, id_column = NULL, file_columns = NULL,
  name = NULL, path = NULL, delete_source = F, ask = T
) {

  # TODO: Cleanup
  # workspace <- "WS1"
  # source_path <- "../DOWNLOADS/systemPipeR/inst/unitTests/"
  # files <- NULL
  # pattern <- ".*\\.fastq$"
  # name <- NULL
  # path <- "RAWREADS"
  # delete_source <- F
  # ask <- F

  LTE <- .lte_env.()

  # Make workspace /////////////////////////////////////////////////////////////

  # Workspace must be registered???   TODO: allow to define new workspace
  chk <- is_registered(LTE$workspaces, x = workspace, error = T)

  # Create workspace if necessary
  chk <- with(LTE$workspaces, is_created[match(workspace, name)])
  if(! chk) create_workspace(workspace)

  # Make dataset definition ////////////////////////////////////////////////////

  dts <- LT_Dataset()

  # Name the dataset
  if(! is.null(name)) {
    dts$name <- name                   # case  1: name
  } else if(! is.null(path)) {
    dts$name <- basename(path)         # case  2: path
  } else {
    dts$name <- paste0("DTS_", dts$id) # default: id
  }
  chk <- ! dts$name %in% list_datasets(workspace)
  checklist(chk, dts$name, "dataset name already exists")

  # Destination path
  if(is.null(path)) path <- dts$name

  # Make default meta data
  dts$MD$path    <- path
  dts$is_primary <- T
  dts$MD$source  <- list(annotations = NULL, files = NULL)
  dts$MD$keys    <- list(id = NULL, files = NULL)

  # Create destination path
  path <- with(LTE$workspaces, path[match(workspace, name)])
  path <- make_path(path, dts$MD$path)
  dir.create(path, showWarnings = F)

  # Make annotations ///////////////////////////////////////////////////////////

  ann <- NULL

  if(! is.null(annotations)) {

    chk <- file.exists(annotations)
    checklist(chk, annotations, "missing annotation file")

    ann <- read.delim(annotations, stringsAsFactors = F)

    # Update meta data
    dts$MD$source$annotations <- annotations

    if(! is.null(id_column)) {
      chk <- is_key(ann, id_column, error = T)
      dts$MD$keys$id <- id_column
    } else stop("missing id_column")

    if(! is.null(file_columns)) {
      chk <- is_key(ann, file_columns, error = T)
      dts$MD$keys$files <- file_columns
    } else stop("missing file_columns")

    dts$TD <- ann
  }

  # Identify input files ///////////////////////////////////////////////////////

  if(is.null(source_path) & is.null(files)) source_path <- "."
  source_path <- as.character(source_path)
  files  <- as.character(files)

  flp <- character(0)

  chk <- 0
  chk <- chk + 1 * (length(files) > 0)
  chk <- chk + 2 * (length(source_path) == 1)
  chk <- chk + 4 * (! is.null(ann))

  # Source: files
  if(chk == 1) flp <- files

  # Source: path
  if(chk == 2) {
    flp <- dir(source_path, include.dirs = F)
    if(! is.null(pattern)) {
      flp <- flp[grepl(pattern, flp, perl = T, ignore.case = F)]
    }
    flp <- paste0(source_path, "/", flp)
  }

  # Source: source_path/files
  if(chk == 3) {
    flp <- sapply(paste0(source_path, "/", flp), make_path)
  }

  # Source: source_path/ann[, "file"]
  if(bitAnd(4, chk)) {
    flp <- with(dts, TD[, MD$keys$files])
    flp <- sapply(paste0(source_path, "/", flp), make_path)
  }

  # Source: annotation + file column
  flp <- as.character(sapply(flp, make_path))
  chk <- file.exists(flp)
  checklist(chk, flp, "missing files")

  # Update dataset definition //////////////////////////////////////////////////

  dts$MD$source$files <- flp

  if(is.null(ann)) {
    dts$MD$keys$id <- "name"
    dts$MD$keys$files <- "file"
    dts$TD <- data.frame(
      name = basename(flp), file = basename(flp), stringsAsFactors = F
    )
  }

  # Copy data files ////////////////////////////////////////////////////////////

  file.copy(from = flp, to = path, overwrite = F, copy.date = T)

  if(delete_source) {
    del <- T
    if(ask) {
      message("Files to be deleted")
      txt_out(flp, sep = "\n", indent = 2)
      del <- confirm_execution(q = F)
    }
    if(del) file.remove(flp)
  }

  # Register dataset definition ////////////////////////////////////////////////

  # Save dataset definition
  path <- lt_path(dts, workspace, LTE$config)
  lt_save(dts, path)

  # Store dataset definition
  env <- globalenv()
  env[[workspace]]@datasets[[dts$name]] <- dts

  # Register dataset
  x <- list(
    id = dts$id, workspace = workspace, name = dts$name, items = nrow(dts$TD),
    is_primary = dts$is_primary, path = dts$MD$path, lt_path = path
  )
  LTE$datasets <- rbind(LTE$datasets, x, stringsAsFactors = F)
}
# =============================================================================.
# Delete dataset definition and associated data from a workspace folder
# -----------------------------------------------------------------------------.
delete_dataset <- function(name, workspace = NULL) {

  LTE <- .lte_env.()

}

# =============================================================================.
# TODO: make sure the workspaces are opened
# -----------------------------------------------------------------------------.
apply2data <- function(
  executor, fun, workspace = NULL, dataset = NULL, element = NULL, ...
) {

  env <- globalenv()

  id <- NULL

  chk <- 0
  chk <- chk +  1 * (length(id) > 0)
  chk <- chk +  2 * (length(dataset) == length(workspace))
  chk <- chk +  4 * (length(workspace) == 1)
  chk <- chk +  8 * (length(dataset) == 1)
  chk <- chk + 16 * (length(workspace) > 1)
  chk <- chk + 32 * (length(dataset) > 1)

  # ids
  if(chk == 1) stop("not implemented")
  # one or several workspaces, no dataset
  if(chk %in% c(4, 16)) {
    chk <- 0
    for(i in 1:length(workspace)) {
      for(dataset in names(env[[workspace[i]]]@datasets)) {
        executor(workspace[i], env[[workspace]]@datasets[[dataset]], fun, ...)
      }
    }
  }
  # {workspace, dataset} one to one
  if(chk %in% c(2 + 16 + 32)) {
    chk <- 0
    for(i in 1:length(workspace)) {
      idx <- which_dataset(workspace = workspace[i], name = dataset[i])
      executor(workspace[i], env[[workspace[i]]]@datasets[[idx]], fun, ...)
    }
  }
  # one workspace, one datasets
  if(chk == 2 + 4 + 8) {
    chk <- 0
    idx <- which_dataset(workspace = workspace, name = dataset)
    executor(workspace, env[[workspace]]@datasets[[idx]], fun, ...)
  }
  # one workspace, several datasets
  if(chk == 4 + 32) {
    chk <- 0
    idx <- which_dataset(workspace = workspace, name = dataset)
    for(i in idx) executor(workspace, env[[workspace]]@datasets[[i]], fun, ...)
  }
  if(chk != 0) stop("incorrect arguments")
}

# =============================================================================.
# Load data from workspace/dataset folders into the workspace environment
# -----------------------------------------------------------------------------.
load_data <- function(
  workspace = NULL, dataset = NULL, element = NULL, reader = read.delim, ...
) {

  LTE <- .lte_env.()

  rd <- function(wks, dts, fun, ...) {
    env <- globalenv()
    flp <- paste0(self_path(env[[wks]]), "/", elements_path(dts))
    env[[wks]][[dts$name]] <- lapply(flp, FUN = fun, ...)
    names(env[[wks]][[dts$name]]) <- elements_name(dts)
  }

  apply2data(executor = rd, fun = reader, workspace, dataset, element, ...)
}
# =============================================================================.
# Save data from the workspace environment into workspace/dataset folders
# -----------------------------------------------------------------------------.
save_data <- function(
  workspace = NULL, dataset = NULL, element = NULL, writer = write.table, ...
) {

  LTE <- .lte_env.()

  wd <- function(wks, dts, fun, ...) {
    env <- globalenv()
    flp <- paste0(self_path(env[[wks]]), "/", elements_path(dts))
    for(lbl in elements_name(dts)) {
      writer(env[[wks]][[dts$name]][[lbl]], flp, ...)
    }
  }

  apply2data(executor = wd, fun = writer, workspace, dataset, element, ...)
}
# =============================================================================.
# Remove (unload) data from the workspace environment
# -----------------------------------------------------------------------------.
close_data <- function(
  workspace = NULL, dataset = NULL, element = NULL, writer = write.table, ...
) {

  LTE <- .lte_env.()

  cd <- function(wks, dts, fun, ...) {
    env <- globalenv()
    env[[wks]][[dts$name]] <- NULL
  }

  apply2data(executor = cd, fun = NULL, workspace, dataset, element, ...)
}


