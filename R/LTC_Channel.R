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
  "LT_Channel", contains = "environment"
)
# =============================================================================.
#' LT_Channel
# -----------------------------------------------------------------------------.
#' @export
#' @description
#'
#' @param ...
#'
#' @return
#' an object of class LT_Channel
# -----------------------------------------------------------------------------.
LT_Workspace <- function(...) {

  obj <- new("LT_Channel")
  obj
}
# -----------------------------------------------------------------------------.

# METHODS ######################################################################

# > Channel ####################################################################


