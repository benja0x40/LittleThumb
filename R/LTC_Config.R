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


# S3 DEFINTION #################################################################

# =============================================================================.
# Inherits from: LT_Data
# =============================================================================.
# LT_Config
# -----------------------------------------------------------------------------.
#' @description
#' @param ...
#'
#' @return
#' an object of class LT_Config
# -----------------------------------------------------------------------------.
LT_Config <- function(...) {

  obj <- LT_Data(...)
  obj <- add_class(obj, "LT_Config")
  obj
}
# -----------------------------------------------------------------------------.
is.LT_Config <- function(x) { inherits(x, "LT_Config") }
# -----------------------------------------------------------------------------.

# METHODS ######################################################################

# > FileSystem #################################################################

# =============================================================================.
#' make_path
# -----------------------------------------------------------------------------.
#' @export
#' @description path assembly
#' @param obj
#' @param name
#' @param root
#' @param key
#' @param value
#'
#' @return path
# -----------------------------------------------------------------------------.
make_path.LT_Config <- function(obj, name, root = "", key = 1, value = 2) {

  make_path(
    obj = obj$TD, paths = obj$MD$paths,
    name = name, root = root, key = key, value = value
  )
}
# =============================================================================.
#' list_paths
# -----------------------------------------------------------------------------.
#' @export
#' @param obj
#' @param x
#' @param key
#'
#' @return path names
# -----------------------------------------------------------------------------.
list_paths.LT_Config <- function(obj, x = NULL, key = 1) {
  lst <- td_selector(obj$TD, x, v = key)
  lst
}
# =============================================================================.
#' create_paths
# -----------------------------------------------------------------------------.
#' @export
#' @param obj
#' @param x
#' @param root
#' @param key
#' @param value
#'
#' @return NULL
# -----------------------------------------------------------------------------.
create_paths.LT_Config <- function(
  obj, x = NULL, root = "", key = 1, value = 2
) {

  lst <- list_paths(obj, x, key = key)
  lst <- make_paths(obj, name = lst, root = root, key = key, value = value)

  for(flp in lst) dir.create(flp, showWarnings = F, recursive = T)
}
# =============================================================================.
#' check_paths
# -----------------------------------------------------------------------------.
#' @export
#' @param obj
#' @param x
#' @param root
#' @param key
#' @param value
#' @param error
#'
#' @return logical
# -----------------------------------------------------------------------------.
check_paths.LT_Config <- function(
  obj, x = NULL, root = "", key = 1, value = 2, error = F
) {

  lst <- list_paths(obj, x, key = key)
  lst <- make_paths(obj, name = lst, root = root, key = key, value = value)

  chk <- file.exists(flp)
  if(error) checklist(chk, lst, msg = "missing paths")

  all(chk)
}
