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


# S4 DEFINTION #################################################################

# =============================================================================.
# Inherits from: environment
# =============================================================================.
trash <- setClass(
  "LT_Workspace", contains = "environment",
  representation = list(
    name = "character", path = "character", datasets = "list", jobs = "list"
  ),

  prototype = prototype(name = "", path = "", datasets = list(), jobs = list())
)
# =============================================================================.
#' LT_Workspace
# -----------------------------------------------------------------------------.
#' @description
#'
#' @param ...
#'
#' @return
#' an object of class LT_Dataset
# -----------------------------------------------------------------------------.
LT_Workspace <- function(...) {

  obj <- new("LT_Workspace")
  obj
}
# -----------------------------------------------------------------------------.

# METHODS ######################################################################

# > FileSystem #################################################################

# =============================================================================.
# Path to workspace folder
# -----------------------------------------------------------------------------.
self_path.LT_Workspace <- function(obj) {
  obj@path
}

# > Workspace ##################################################################

# =============================================================================.
#' define_workspace
# -----------------------------------------------------------------------------.
#' @description
#' Add {name, path, is_created, is_opened} to LTE$workspaces register
#'
#' @param name
#' @param path
#'
#' @return
# -----------------------------------------------------------------------------.
define_workspace <- function(name, path) {

  LTE <- .lte_env.()

  chk <- check_class(name, "character", error = T)
  chk <- check_class(path, "character", error = T)

  path <- suppressWarnings(normalizePath(path))

  chk <- is_registered(LTE$workspaces, k = 1, x = name)
  if(chk) stop("duplicate workspace name ", name)

  chk <- is_registered(LTE$workspaces, k = 2, x = path)
  if(chk) stop("duplicate workspace path ", path)

  x <- list(name = name, path = path, is_created = F, is_opened = F)
  LTE$workspaces <- rbind(LTE$workspaces, x, stringsAsFactors = F)

  .lte_save.()
}
# =============================================================================.
# Return workspace names
# -----------------------------------------------------------------------------.
list_workspaces <- function(detailed = F, x = NULL) {

  LTE <- .lte_env.()

  wks <- LTE$workspaces
  if(nrow(wks) > 0) {
    wks <- td_selector(wks, x, 1:ncol(wks))
    if(nrow(wks) > 0 & ! detailed) wks <- wks$name
  } else {
    if(! detailed) wks <- character()
  }

  wks
}
# =============================================================================.
# Create workspace folders
# -----------------------------------------------------------------------------.
create_workspace <- function(name, path = NULL) {

  LTE <- .lte_env.()
  lbl <- name # to avoid ambiguity

  if(! is.null(path)) define_workspace(lbl, path)
  path <- with(LTE$workspaces, path[match(lbl, name)])

  # Create workspace folders
  create_paths(LTE$config, is_dir == T  & level == "workspace", root = path)

  idx <- which(td_selector(LTE$workspaces, name == lbl))
  LTE$workspaces[idx, "is_created"] <- T
}
# =============================================================================.
# Delete workspace folders
# -----------------------------------------------------------------------------.
delete_workspace <- function(name, ask = T) {

  LTE <- .lte_env.()
  lbl <- name # to avoid ambiguity
  flp <- with(LTE$workspaces, path[match(lbl, name)])

  # Ask for confirmation
  del <- ! ask
  if(ask) {
    message("Irreversible deletion of workspace")
    message(flp)
    del <- confirm_execution(q = F)
  }
  if(del & flp != "") {
    # Unregister workspace
    idx <- match(x = lbl, LTE$workspaces$name)
    LTE$workspaces <- LTE$workspaces[- idx, ]
    rownames(LTE$workspaces) <- NULL

    clear_path(flp)
  }
}
# =============================================================================.
# Load workspace environment in R
# -----------------------------------------------------------------------------.
open_workspace <- function(name, path = NULL) {

  LTE <- .lte_env.()
  lbl <- name # to avoid ambiguity

  chk <- td_selector(LTE$workspaces, name == lbl, v = "is_created")
  if(! chk) stop("workspace has to be created before it can be opened")

  chk <- td_selector(LTE$workspaces, name == lbl, v = "is_opened")
  if(! chk) {

    env <- globalenv()
    env[[lbl]] <- LT_Workspace()
    env[[lbl]]@name <- lbl
    env[[lbl]]@path <- td_selector(LTE$workspaces, name == lbl, v = "path")

    register_value(LTE$workspaces, x = lbl, v = "is_opened") <- T
  }

  # Auto load existing dataset and job lists
  if(nrow(LTE$datasets) > 0) {
    dts <- td_selector(LTE$datasets, workspace == lbl, "lt_path")
    if(length(dts) > 0)  env[[lbl]]@datasets <- lapply(dts, lt_load)
    ids <- td_selector(LTE$datasets, workspace == lbl, "id")
    names(env[[lbl]]@datasets) <- ids
  }
}
# =============================================================================.
# Remove workspace environment from R
# -----------------------------------------------------------------------------.
close_workspace <- function(name) {

  LTE <- .lte_env.()
  lbl <- name # to avoid ambiguity

  chk <- is_registered.data.frame(LTE$workspaces, "name", lbl, error = T)
  rm(list = lbl, pos = globalenv())

  register_value(LTE$workspaces, x = lbl, v = "is_opened") <- F
}
