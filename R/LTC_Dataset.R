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
  LTE <- lt_env()
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
# -----------------------------------------------------------------------------.
`self_path<-.LT_Dataset` <- function(obj, path) {
  obj$MD$path <- path
  obj
}

# =============================================================================.
# Path to data files
# -----------------------------------------------------------------------------.
elements_path.LT_Dataset <- function(obj, name = NULL) {
  kid <- obj$MD$keys$id
  kfl <- obj$MD$keys$files
  if(is.null(name)) idx <- rep(T, nrow(obj$TD))
  else idx <- match(name, obj$TD[[kid]])
  obj$TD[[kfl]][idx]
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

  LTE <- lt_env()

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

  LTE <- lt_env()

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
create_dataset <- function(
  workspace, source_path = NULL, files = NULL, pattern = NULL,
  annotations = NULL, id_column = "name", file_columns = "file",
  name = NULL, path = NULL, delete_source = F, ask = T
) {

  LTE <- lt_env()

  # Make workspace /////////////////////////////////////////////////////////////

  chk <- is_registered(LTE$workspaces, x = workspace, error = T)
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

    if(is.data.frame(annotations)) {
      ann <- annotations
      annotations <- "data.frame"
    } else {
      chk <- file.exists(annotations)
      checklist(chk, annotations, "missing annotation file")
      ann <- read.delim(annotations, stringsAsFactors = F)
    }

    # Update meta data
    dts$MD$source$annotations <- annotations

    chk <- is_key(ann, id_column, error = T)
    dts$MD$keys$id <- id_column

    dts$MD$keys$files <- file_columns

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
  chk <- chk + 8 * (file_columns %in% colnames(ann))

  # Source: files
  if(chk %in% c(1, 1 + 4)) flp <- sapply(files, make_path)

  # Source: path
  if(chk %in% c(2, 2 + 4)) {
    flp <- dir(source_path, include.dirs = F)
    if(! is.null(pattern)) {
      flp <- flp[grepl(pattern, flp, perl = T, ignore.case = F)]
    }
    flp <- sapply(paste0(source_path, "/", flp), make_path)
  }

  # Source: path/files
  if(chk %in% c(3, 3 + 4)) {
    flp <- sapply(paste0(source_path, "/", files), make_path)
  }

  # Source: ann[, "file"]
  if(chk == 4 + 8) {
    flp <- with(dts, TD[, MD$keys$files])
    flp <- sapply(flp, make_path)
  }

  # Source: path/ann[, "file"]
  if(chk == 2 + 4 + 8) {
    flp <- with(dts, TD[, MD$keys$files])
    flp <- sapply(paste0(source_path, "/", flp), make_path)
  }

  if(length(flp) == 0) stop("incorrect arguments")

  # Resolved paths
  flp <- as.character(sapply(flp, make_path))
  chk <- file.exists(flp)
  checklist(chk, flp, "missing files")

  # Update dataset definition //////////////////////////////////////////////////

  dts$MD$source$files <- flp

  if(is.null(ann)) {
    ann <- data.frame(basename(flp), basename(flp), stringsAsFactors = F)
    colnames(ann) <- c(id_column, file_columns)
    dts$TD <- ann

  } else {
    dts$TD[[file_columns]] <- basename(flp)
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

  # Update LTE /////////////////////////////////////////////////////////////////
  .lte_save.()
}

# =============================================================================.
# Move/Rename dataset
# -----------------------------------------------------------------------------.
clone_dataset <- function(workspace, dataset, name, path = NULL) {

  LTE <- lt_env()
  env <- globalenv()

  dts <- env[[workspace]]@datasets[[dataset]]
  obj$MD$source <- list(id = dts$id, name = dts$name)
  dts$id <- make_id()
  dts$name <- name
  dts$is_primary <- F
  if(! is.null(path)) self_path(dts) <- path

  # Save the cloned dataset definition
  path <- lt_path(dts, workspace, LTE$config)
  lt_save(dts, path)

  # Store the cloned dataset definition
  env[[workspace]]@datasets[[dts$name]] <- dts
  env[[workspace]][[dts$name]] <- env[[workspace]][[dataset]]

  # Register the cloned dataset
  x <- list(
    id = dts$id, workspace = workspace, name = dts$name, items = nrow(dts$TD),
    is_primary = dts$is_primary, path = dts$MD$path, lt_path = path
  )

  LTE$datasets <- rbind(LTE$datasets, x, stringsAsFactors = F)
}

# =============================================================================.
# Move/Rename dataset
# -----------------------------------------------------------------------------.
move_dataset <- function(workspace, dataset, name = NULL, path = NULL) { }

# =============================================================================.
# Delete dataset definition and associated data from a workspace folder
# -----------------------------------------------------------------------------.
# delete_dataset <- function(workspace, dataset) { }
# clone_dataset <- function(workspace, dataset, name, path = NULL) { }
# merge_datasets <- function(workspace, dataset1, dataset2, name, path = NULL, delete = F) { }

# =============================================================================.
# TODO: make sure the workspaces are opened
# -----------------------------------------------------------------------------.
apply2dataset <- function(
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

  rd <- function(wks, dts, fun, ...) {
    env <- globalenv()
    flp <- paste(
      self_path(env[[wks]]), self_path(dts), elements_path(dts), sep = "/"
    )
    env[[wks]][[dts$name]] <- lapply(flp, FUN = fun, ...)
    names(env[[wks]][[dts$name]]) <- elements_name(dts)
  }
  apply2dataset(executor = rd, fun = reader, workspace, dataset, element, ...)
}
# =============================================================================.
# Move data from workspace/dataset to workspace/dataset
# -----------------------------------------------------------------------------.
move_data <- function(
  workspace = NULL, dataset = NULL, element = NULL, path, ...
) {

  mv <- function(wks, dts, fun, ...) {
    env <- globalenv()
    src <- paste(
      self_path(env[[wks]]), self_path(dts), elements_path(dts), sep = "/"
    )
    tgt <- paste(
      self_path(env[[wks]]), path, elements_path(dts), sep = "/"
    )
    ### execute(paste("mv -f", src, tgt)) ###
    ### env[[wks]][[dts$name]] ###
  }
  apply2dataset(executor = mv, fun = NULL, workspace, dataset, element, ...)
}
# =============================================================================.
# Save data from the workspace environment into workspace/dataset folders
# -----------------------------------------------------------------------------.
save_data <- function(
  workspace = NULL, dataset = NULL, element = NULL, writer = write.table, ...
) {

  wd <- function(wks, dts, fun, ...) {
    env <- globalenv()
    flp <- paste(
      self_path(env[[wks]]), self_path(dts), elements_path(dts), sep = "/"
    )
    for(lbl in elements_name(dts)) {
      writer(env[[wks]][[dts$name]][[lbl]], flp, ...)
    }
  }
  apply2dataset(executor = wd, fun = writer, workspace, dataset, element, ...)
}
# =============================================================================.
# Remove (unload) data from the workspace environment
# -----------------------------------------------------------------------------.
close_data <- function(
  workspace = NULL, dataset = NULL, element = NULL, ...
) {

  cd <- function(wks, dts, fun, ...) {
    env <- globalenv()
    env[[wks]][[dts$name]] <- NULL
  }
  apply2dataset(executor = cd, fun = NULL, workspace, dataset, element, ...)
}


