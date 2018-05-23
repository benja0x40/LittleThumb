# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' List of global options for the LittleThumb package
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DefaultOptions <- function() {
  list(
    # Path generation
    path      = "",
    extension = ".rds",
    relative  = T,

    # Evaluation
    envir = NA,

    # Behavior
    makedir  = T,
    rebuild  = F,
    overload = F,
    remove   = T,

    # Traceability
    messages = T,
    history  = "LittleThumb"
  )
}

# =============================================================================.
#' Global options for LittleThumb functions
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{SaveObj},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @description
#' This function sets the default value of arguments used by the main functions
#' of the LittleThumb package, which allows to control the automated
#' saving/loading operations performed by these functions at the global level.
#'
#' @param ...
#' Any of the following arguments:
#'
#' @param path
#' default folder used to save R objects.
#'
#' @param extension
#' RDS file extension (default = ".rds").
#'
#' @param relative
#' logical value. When \code{TRUE} the default folder defines the root path for
#' all read/write operations automated by LittleThumb, meaning that any path
#' specified when calling \code{MakeObj}, \code{SaveObj} or \code{LoadObj}
#' will represent a sub path of the default folder.
#'
#' @param envir
#' environment containing R objects for \code{MakeObj}, \code{SaveObj} and
#' \code{LoadObj}. With the default value (\code{NA}), this environment is
#' the parent.frame of called LittleThumb functions.
#'
#' @param makedir
#' logical value, if \code{TRUE} non-existing folders are created automatically
#' when necessary.
#'
#' @param rebuild
#' logical value, if \code{TRUE} forces \code{MakeObj} to regenerate R objects
#' even if when the associated RDS files already exist.
#'
#' @param overload
#' logical value, if \code{TRUE} forces \code{MakeObj} and \code{LoadObj} to
#' load R objects from associated RDS files even when these objects already
#' exist in the R environment.
#'
#' @param remove
#' TODO: documentation
#'
#' @param messages
#' logical value enabling or disabling status messages from LittleThumb
#' functions (default = T, yes).
#'
#' @param history
#' TODO: documentation
# -----------------------------------------------------------------------------.
#' @export
LittleThumb <- function(...) {

  opt <- names(LittleThumb::DefaultOptions())

  cfg <- list(...)
  cfg <- cfg[names(cfg) %in% opt]
  cfg <- cfg[! sapply(cfg, is.null)]

  if(length(cfg) > 0) {
    names(cfg) <- paste0("LittleThumb.", names(cfg))
    options(cfg)
  } else {
    cfg <- options()[paste0("LittleThumb.", opt)]
    names(cfg) <- gsub( "^LittleThumb\\.", "", names(cfg))
    cfg
  }
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Remove global options of the LittleThumb package from the R environment
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RemoveOptions <- function() {
  cfg <- options()
  cfg <- cfg[grepl( "^LittleThumb\\.", names(cfg))]
  cfg[] <- vector("list", length(cfg))
  options(cfg)
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Reinitialize global options of the LittleThumb package
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ResetOptions <- function() {
  do.call(LittleThumb, LittleThumb::DefaultOptions())
}
