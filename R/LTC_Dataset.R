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
# LT_Dataset
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

# > FileSystem : self definition ###############################################

# =============================================================================.
# Path to dataset definition
# -----------------------------------------------------------------------------.
lt_path.LT_Dataset <- function(
  obj, workspace, config = lt_cfg(), path_label = "DTSDIR"
) {
  LTE <- lt_env()
  path <- with(LTE$workspaces, path[match(workspace, name)])
  path <- make_path(path, make_path(config, path_label), obj$name, ext = ".txt")
  path
}

# > FileSystem : self location #################################################

# =============================================================================.
# Path to dataset folder
# -----------------------------------------------------------------------------.
self_path.LT_Dataset <- function(obj) { obj$MD$path }
# -----------------------------------------------------------------------------.
`self_path<-.LT_Dataset` <- function(obj, value) {

  LTE <- lt_env()

  # Update path in the dataset register
  i <- which_dataset(id = obj$id)
  if(length(i) > 0) LTE$datasets$path[i] <- value

  obj$MD$path <- value

  obj
}

# > FileSystem : elements ######################################################

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
elements_name.LT_Dataset <- function(obj) {
  kid <- obj$MD$keys$id
  obj$TD[[kid]]
}

# > Dataset ####################################################################

# =============================================================================.
#' create_dataset
# -----------------------------------------------------------------------------.
# Use cases:
# - from a whole folder
# - from files within a folder
# - from a given list of files
# -----------------------------------------------------------------------------.
#' @description
#' Create dataset definition and copy associated data inside a workspace folder
#'
#' @param workspace
#' @param source_path
#' @param files
#' @param pattern
#' @param annotations
#' @param id_column
#' @param file_columns
#' @param name
#' @param path
#' @param delete_source
#' @param ask
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
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
  dts$MD$source  <- list(annotations = NULL, files = NULL)
  dts$MD$keys    <- list(id = NULL, files = NULL)

  # Create destination path
  path <- with(LTE$workspaces, path[match(workspace, name)])
  path <- make_path(path, dts$MD$path)
  dir.create(path, recursive = T, showWarnings = F)

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
    dts$MD$keys$id <- id_column
    dts$MD$keys$files <- file_columns
  } else {
    dts$TD[[file_columns]] <- basename(flp)
  }

  # Copy data files ////////////////////////////////////////////////////////////

  # TODO: better management should be done here
  chk <- ! basename(flp) %in% dir(path)
  if(any(chk)) {
    message("Copying files...")
    file.copy(from = flp[chk], to = path, overwrite = F, copy.date = T)
  }

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
  env[[workspace]]@datasets[[dts$id]] <- dts

  # Register dataset
  x <- list(
    id = dts$id, workspace = workspace, name = dts$name, items = nrow(dts$TD),
    path = dts$MD$path, lt_path = path
  )
  LTE$datasets <- rbind(LTE$datasets, x, stringsAsFactors = F)

  # Update LTE /////////////////////////////////////////////////////////////////
  .lte_save.()
}

# =============================================================================.
#' list_datasets
# -----------------------------------------------------------------------------.
#' @description
#' Return registered dataset names
#'
#' @param workspace
#' @param detailed
#' @param x
#'
#' @return dataset names
# -----------------------------------------------------------------------------.
#' @export
list_datasets <- function(workspace = NULL, detailed = F, x = NULL) {

  LTE <- lt_env()

  if(is.null(workspace)) workspace <- list_workspaces()
  checklist(
    workspace %in% LTE$workspaces$name, workspace, msg = "undefined workspace"
  )

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
#' which_dataset
# -----------------------------------------------------------------------------.
#' @description
#' Return the register index of datasets
#'
#' @param id
#' @param workspace
#' @param name
#'
#' @return integer
# -----------------------------------------------------------------------------.
#' @export
which_dataset <- function(id = NULL, workspace = NULL, name = NULL) {

  LTE <- lt_env()

  # TODO: should be based on reg_idx in data.frame Registration interface
  idx <- NULL
  if(! is.null(workspace)) {
    idx <- which(LTE$datasets$workspace %in% workspace)
  }
  if(! is.null(name)) {
    k <- which(LTE$datasets$name %in% name)
    if(! is.null(idx)) k <- base::intersect(idx, k)
    idx <- k
  }
  if(! is.null(id)) {
    k <- which(LTE$datasets$id %in% id)
    if(! is.null(idx))  k <- base::intersect(idx, k)
    idx <- k
  }
  if(is.null(idx)) idx <- integer()

  idx
}

# =============================================================================.
#' dataset_object
# -----------------------------------------------------------------------------.
#' @description
#' Return registered dataset names
#'
#' @param id
#' @param workspace
#' @param name
#'
#' @return object of class LT_Dataset
# -----------------------------------------------------------------------------.
#' @export
dataset_object <- function(id = NULL, workspace = NULL, name = NULL) {

  LTE <- lt_env()

  dts <- NULL
  idx <- which_dataset(id, workspace, name)
  if(length(idx) > 1) stop("query matches multiple datasets")
  if(length(idx) > 0) {
    lti <- LTE$datasets$id[idx]
    wks <- LTE$datasets$workspace[idx]
    chk <- with(LTE$workspaces, is_opened[match(wks, name)])
    if(! chk) stop("workspace is not opened: ", wks)
    wks <- globalenv()[[wks]]
    dts <- wks@datasets[[lti]]
    if(is.null(dts)) dts <- lt_load(LTE$datasets$lt_path[idx])
  }

  dts
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
#' @export
dataset_md <- function(id = NULL, workspace = NULL, name = NULL) {
  dts <- dataset_object(id, workspace, name)
  dts$MD
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
#' @export
dataset_td <- function(id = NULL, workspace = NULL, name = NULL) {
  dts <- dataset_object(id, workspace, name)
  dts$TD
}

# =============================================================================.
#' clone_dataset
# -----------------------------------------------------------------------------.
#' @param workspace
#' @param dataset
#' @param name
#' @param path
#' @param files
#' @param file_columns
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
clone_dataset <- function(
  workspace, dataset, name, path = NULL, files = NULL, file_columns = NULL
) {

  LTE <- lt_env()
  env <- globalenv()

  idx <- which_dataset(workspace = workspace, name = dataset)
  dataset_id <- LTE$datasets$id[idx]

  dts <- env[[workspace]]@datasets[[dataset_id]]
  dts$MD$source <- list(id = dts$id, name = dts$name)
  dts$id <- make_id()
  dts$name <- name
  if(! is.null(path)) {
    self_path(dts) <- path
    path <- make_path(self_path(env[[workspace]]), path)
    dir.create(path, recursive = T, showWarnings = F)
  }

  # Data binding
  if(! is.null(file_columns)) {
    dts$MD$keys$files <- file_columns
  }
  if(! is.null(files)) {
    dts$TD[[dts$MD$keys$files]] <- files
  }

  # Save the cloned dataset definition
  path <- lt_path(dts, workspace, LTE$config)
  lt_save(dts, path)

  # Store the cloned dataset definition
  env[[workspace]]@datasets[[dts$id]] <- dts

  # Copy loaded data
  if(! is.null(env[[workspace]][[dataset]])) {
    env[[workspace]][[dts$name]] <- env[[workspace]][[dataset]]
  }

  # Register the cloned dataset
  x <- list(
    id = dts$id, workspace = workspace, name = dts$name, items = nrow(dts$TD),
    path = dts$MD$path, lt_path = path
  )

  LTE$datasets <- rbind(LTE$datasets, x, stringsAsFactors = F)

  # Update LTE /////////////////////////////////////////////////////////////////
  .lte_save.()
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
# change_dataset_path <- function(workspace, dataset, path) {
#
#   LTE <- lt_env()
#   env <- globalenv()
#
#   dataset_id <- td_selector(
#     LTE$datasets, workspace == workspace & name == dataset, "id"
#   )
#   self_path(env[[workspace]]@datasets[[dataset_id]]) <- path
# }

# =============================================================================.
# Move/Rename dataset
# -----------------------------------------------------------------------------.
move_dataset <- function(workspace, dataset, name = NULL, path = NULL) { }

# =============================================================================.
# Delete dataset definition and associated data from a workspace folder
# -----------------------------------------------------------------------------.
# delete_dataset <- function(workspace, dataset) { }
# merge_datasets <- function(workspace, dataset1, dataset2, name, path = NULL, delete = F) { }

# =============================================================================.
#' apply2dataset
# -----------------------------------------------------------------------------.
# TODO: make sure the workspaces are opened
# -----------------------------------------------------------------------------.
#' @param executor
#' @param fun
#' @param workspace
#' @param dataset
#' @param element
#' @param ...
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
apply2dataset <- function(
  executor, fun, workspace = NULL, dataset = NULL, element = NULL, ...
) {

  LTE <- lt_env()

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
    for(j in 1:length(workspace)) {
      idx <- which_dataset(workspace = workspace[j])
      for(i in idx) {
        dts <- dataset_object(id = LTE$datasets$id[i])
        executor(workspace[j], dts, fun, ...)
      }
    }
  }
  # {workspace, dataset} one to one
  if(chk %in% c(2 + 16 + 32)) {
    chk <- 0
    for(i in 1:length(workspace)) {
      dts <- dataset_object(workspace = workspace[i], name = dataset[i])
      executor(workspace[i], dts, fun, ...)
    }
  }
  # one workspace, one datasets
  if(chk == 2 + 4 + 8) {
    chk <- 0
    dts <- dataset_object(workspace = workspace, name = dataset)
    executor(workspace, dts, fun, ...)
  }
  # one workspace, several datasets
  if(chk == 4 + 32) {
    chk <- 0
    idx <- which_dataset(workspace = workspace, name = dataset)
    for(i in idx) {
      dts <- dataset_object(id = LTE$datasets$id[i])
      executor(workspace, dts, fun, ...)
    }
  }
  if(chk != 0) stop("incorrect arguments")
}

# =============================================================================.
#' load_data
# -----------------------------------------------------------------------------.
#' @description
#' Load data from workspace/dataset folders into the workspace environment
#'
#' @param workspace
#' @param dataset
#' @param element
#' @param reader
#' @param ...
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
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
  message("Loading data...")
  apply2dataset(executor = rd, fun = reader, workspace, dataset, element, ...)
}

# =============================================================================.
#' save_data
# -----------------------------------------------------------------------------.
#' @description
#' Save data from the workspace environment into workspace/dataset folders
#'
#' @param workspace
#' @param dataset
#' @param element
#' @param writer
#' @param ...
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
save_data <- function(
  workspace = NULL, dataset = NULL, element = NULL, writer = write.table, ...
) {

  wd <- function(wks, dts, fun, ...) {
    env <- globalenv()
    flp <- paste(
      self_path(env[[wks]]), self_path(dts), elements_path(dts), sep = "/"
    )
    names(flp) <- elements_name(dts)
    for(lbl in elements_name(dts)) {
      writer(env[[wks]][[dts$name]][[lbl]], flp[lbl], ...)
    }
  }
  message("Saving data...")
  apply2dataset(executor = wd, fun = writer, workspace, dataset, element, ...)
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


