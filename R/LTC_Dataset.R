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
#' @export
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
#
# -----------------------------------------------------------------------------.
lt_path.LT_Dataset <- function(obj, workspace, config, path_label = "DTSDIR") {
  LTE <- .lte_env.()
  path <- with(LTE$workspaces, path[match(workspace, name)])
  path <- make_path(path, make_path(config, path_label), dts$name, ext = ".txt")
  path
}

# > Dataset ####################################################################

# =============================================================================.
# Return dataset names
# -----------------------------------------------------------------------------.
list_datasets <- function(workspace = NULL, x, detailed = F) {

  LTE <- .lte_env.()

  if(is.null(workspace)) {
    wks <- list_workspaces(is_opened == T)
    if(nrow(wks) == 0) wks <- list_workspaces()
  } else {
    # verify if workspace is registered
  }

  dts <- LTE$datasets
  if(nrow(dts) > 0) {
    if(! detailed) dts <- dts$name
  }

  dts

  # by default workspace = opened workspaces | all workspaces if none is opened
  # x is a logical expression (lazy eval)
  # use td_selector on LTE$datasets
}
# =============================================================================.
0
# Create dataset definition and copy associated data inside a workspace folder
# -----------------------------------------------------------------------------.
# Use cases:
# - from a whole folder
# - from files within a folder
# - from a given list of files
# -----------------------------------------------------------------------------.
create_dataset <- function(
  workspace, source_path = NULL, files = NULL, pattern = NULL,
  annotation = NULL, file_columns = NULL, id_columns = NULL,
  name = NULL, path = NULL, delete_source = F, ask = T
) {

  workspace <- "WS1"
  source_path <- "../DOWNLOADS/systemPipeR/inst/unitTests/"
  files <- NULL
  pattern <- ".*\\.fastq$"
  name <- NULL
  path <- "RAWREADS"
  delete_source <- F
  ask <- F

  LTE <- .lte_env.()

  # Target workspace ///////////////////////////////////////////////////////////

  # Workspace must be registered
  chk <- is_registered(LTE$workspaces, x = workspace, error = T)

  # Create workspace if necessary
  chk <- with(LTE$workspaces, is_created[match(workspace, name)])
  if(! chk) create_workspace(workspace)

  # Identify input files ///////////////////////////////////////////////////////

  if(is.null(source_path) & is.null(files)) source_path <- "."
  source_path <- as.character(source_path)
  files  <- as.character(files)

  flp <- character(0)

  chk <- 0
  chk <- chk + 1 * (length(files) > 0)
  chk <- chk + 2 * (length(source_path) == 1)

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

  flp <- as.character(sapply(flp, make_path))
  chk <- file.exists(flp)
  checklist(chk, flp, "missing files")

  # Make dataset object ////////////////////////////////////////////////////////

  # Make minimal creation procedure (+ retrieve ideas below)
  # and then complexify procedure to include annotations and key columns

  dts <- LT_Dataset()

  # Name dataset
  if(! is.null(name)) {
    dts$name <- name              # case 1: name
  } else if(! is.null(path)) {
    dts$name <- basename(path)  # case 2: path
  } else {
    dts$name <- paste0("DTS_", dts$id)            # default: id
  }

  # Make destination path
  if(is.null(path)) path <- dts$name

  dts$MD$path <- path
  dts$is_primary <- T

  path <- with(LTE$workspaces, path[match(workspace, name)])
  path <- make_path(path, dts$MD$path)
  dir.create(path, showWarnings = F)

  # Copy files
  file.copy(from = flp, to = path, overwrite = F, copy.date = T)

  if(delete_source) {
    del <- T
    if(ask) {
      message("Files to be deleted")
      txt_out(flp, sep = "\n", indent = 2)
      del <- confirm_execution(q = F)
    }
    if(del) file.remove(files)
  }

  # Register dataset object ////////////////////////////////////////////////////

  dts$MD$source <- list(files = flp)
  dts$TD <- data.frame(
    name = basename(flp), file = basename(flp), stringsAsFactors = F
  )

  # Save dataset definition
  path <- lt_path(dts, workspace, LTE$config)
  lt_save(dts, path)

  # Store dataset definition
  env <- globalenv()
  env[[workspace]]@datasets[[dts$name]] <- dts

  # Register dataset
  x <- list(
    id = dts$id, workspace = workspace, name = dts$name, path = dts$MD$path,
    is_primary = dts$is_primary
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
# Load data into a workspace environment
# -----------------------------------------------------------------------------.
open_dataset <- function(name, workspace = NULL) {

  LTE <- .lte_env.()

}
# =============================================================================.
# Remove data from a workspace environment
# -----------------------------------------------------------------------------.
close_dataset <- function(name, workspace = NULL) {

  LTE <- .lte_env.()

}


