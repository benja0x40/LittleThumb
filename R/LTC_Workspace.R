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
# LT_Workspace
# -----------------------------------------------------------------------------.
LT_Workspace <- function(...) {

  obj <- new("LT_Workspace")
  obj
}
# -----------------------------------------------------------------------------.
is.LT_Workspace <- function(x) { inherits(x, "LT_Workspace") }
# -----------------------------------------------------------------------------.

# METHODS ######################################################################

# > FileSystem : self location #################################################

# =============================================================================.
# Path to workspace folder
# -----------------------------------------------------------------------------.
self_path.LT_Workspace <- function(obj) { obj@path }
# -----------------------------------------------------------------------------.
`self_path<-.LT_Workspace` <- function(obj, value) {

  LTE <- lt_env()

  # Update path in the workspace register
  i <- match(obj@name, LTE$workspaces$name)
  if(! is.na(i)) LTE$workspaces$path[i] <- value

  obj@path <- value

  obj
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
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
define_workspace <- function(name, path) {

  LTE <- lt_env()

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
# list_workspaces
# -----------------------------------------------------------------------------.
#' @param detailed
#'
#' @param x
#'
#' @return workspace names
# -----------------------------------------------------------------------------.
#' @export
list_workspaces <- function(detailed = F, x = NULL) {

  LTE <- lt_env()

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
#' which_dataset
# -----------------------------------------------------------------------------.
#' @description
#' Return the register index of workspaces
#'
#' @param name
#'
#' @return integer
# -----------------------------------------------------------------------------.
#' @export
which_workspace <- function(name = NULL) {

}
# =============================================================================.
#' workspace_object
# -----------------------------------------------------------------------------.
#' @description
#' Return a registered workspace object
#'
#' @param name
#'
#' @return object of class LT_Workspace
# -----------------------------------------------------------------------------.
#' @export
workspace_object <- function(name = NULL) {

}
# =============================================================================.
#' create_workspace
# -----------------------------------------------------------------------------.
#' @description Create workspace folders
#' @param name
#'
#' @param path
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
create_workspace <- function(name, path = NULL) {

  LTE <- lt_env()
  lbl <- name # to avoid ambiguity

  if(! is.null(path)) define_workspace(lbl, path)
  path <- with(LTE$workspaces, path[match(lbl, name)])

  # Create workspace folders
  create_paths(LTE$config, is_dir == T  & level == "workspace", root = path)

  idx <- which(td_selector(LTE$workspaces, name == lbl))
  LTE$workspaces[idx, "is_created"] <- T

  .lte_save.()
}
# =============================================================================.
#' delete_workspace
# -----------------------------------------------------------------------------.
#' @description Delete workspace folders
#' @param name
#'
#' @param ask
# -----------------------------------------------------------------------------.
#' @export
delete_workspace <- function(name, ask = T) {

  LTE <- lt_env()
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

  .lte_save.()
}
# =============================================================================.
#' open_workspace
# -----------------------------------------------------------------------------.
#' @description Load workspace environment in R
#' @param name
#' @param path
#' @param reopen
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
open_workspace <- function(name, path = NULL, reopen = F) {

  LTE <- lt_env()
  env <- globalenv()
  lbl <- name # to avoid ambiguity

  chk <- td_selector(LTE$workspaces, name == lbl, v = "is_created")
  if(! chk) stop("workspace has to be created before it can be opened")

  chk <- td_selector(LTE$workspaces, name == lbl, v = "is_opened")
  if(reopen | ! chk) {
    env[[lbl]] <- LT_Workspace()
    env[[lbl]]@name <- lbl
    env[[lbl]]@path <- td_selector(LTE$workspaces, name == lbl, v = "path")

    register_value(LTE$workspaces, x = lbl, v = "is_opened") <- T
  }

  # Auto load existing datasets
  if(nrow(LTE$datasets) > 0) {
    dts <- td_selector(LTE$datasets, workspace == lbl, "lt_path")
    if(length(dts) > 0)  env[[lbl]]@datasets <- lapply(dts, lt_load)
    ids <- td_selector(LTE$datasets, workspace == lbl, "id")
    names(env[[lbl]]@datasets) <- ids
  }
}
# =============================================================================.
#' close_workspace
# -----------------------------------------------------------------------------.
#' @description Remove workspace environment from R
#' @param name
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
close_workspace <- function(name) {

  LTE <- lt_env()
  lbl <- name # to avoid ambiguity

  chk <- is_registered.data.frame(LTE$workspaces, "name", lbl, error = T)
  rm(list = lbl, pos = globalenv())

  register_value(LTE$workspaces, x = lbl, v = "is_opened") <- F
}
