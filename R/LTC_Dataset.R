# INCLUDES #####################################################################

# Generic and regular functions
#' @include LTF_Basics.R
#' @include LTF_R_Objects.R
#' @include LTF_Registration.R
#' @include LTF_TextRepresentation.R
#' @include LTF_FileSystem.R
#' @include LTF_Workspace.R

# Class definitions & implementations
#' @include LTC_TabularData.R
#' @include LTC_MetaData.R
#' @include LTC_Data.R
#' @include LTC_Config.R

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

# > Dataset ####################################################################

# =============================================================================.
# Use cases:
# - from files within a folder
# - from a given list of files
#
# -----------------------------------------------------------------------------.
create_dataset <- function(
  src, files = NULL, destination, name = NULL, ann = NULL, workspace = NULL, delsrc = F
) {

  LTE <- .lte_env.()

  chk <- td_selector(LTE$workspaces, name == name, v = "is_opened")


  # if(is.null(workspace)) workspace <- CURRENT WORKSPACE
  # setwd(rootpath(workspace))

  lt_id <- lt_id()
  if(is.null(name)) name <- paste0("Dataset_", lt_id)

  folder <- paste0(workspace, "/", destdir)

  # Make sure data files are available
  chk <- file.exists(srcfiles)
  checklist(chk, srcfiles, "missing files")

  # TODO: detect issues with srcdir, destdir, and workspace
  srcdir <- unique(dirname(srcfiles))

  # TODO output in commands.sh?
  cmd <- c()
  if(! file.exists(path)) cmd <- c(cmd, paste("mkdir -p", path))

  if(action == "copy") cmd <- c(cmd, paste("cp", path))
  execute(cmd)

  prp <- list(
    lt_id = lt_id(),
    name  = make.names(name),
    primary = T,
    workspace = workspace,
    data_path = data_path
    # source = list(workspace = NA, dataset = NA, job = NA)
  )
  if(is.null(ann)) {
    ann <- DataFrame(
      file = basename(files)
    )
  }
  new("LT_Description", properties = prp, df = ann)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
list_datasets <- function(workspace = NULL, x, detailed = F) {

  LTE <- .lte_env.()

    # by default workspace = opened workspaces | all workspaces if none is opened
    # x is a logical expression (lazy eval)
    # use td_selector on LTE$datasets
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
delete_dataset <- function(name) {

  LTE <- .lte_env.()

}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
open_dataset <- function(name) {

  LTE <- .lte_env.()

}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
close_dataset <- function(name) {

  LTE <- .lte_env.()

}


