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
    # Path generation (SaveObj, LoadObj, DeleteObj, AvailableObj)
    path      = "",            #
    extension = ".rds",        #
    relative  = T,             # + MakePath

    # Evaluation (SaveObj, LoadObj, DeleteObj, MakeObj)
    envir = NA,

    # Behavior
    makedir  = T,              # SaveObj <= MakeObj
    overload = F,              # LoadObj <= MakeObj
    rebuild  = F,              # MakeObj
    cleanup  = T,              # MakeObj
    remove   = T,              # DeleteObj

    # Traceability
    messages = T,              # SaveObj, LoadObj, DeleteObj, MakeObj
    history  = "LittleThumb"   # TODO:
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
#' environment containing the R object.
#' With the default value (\code{NA}), this environment is the
#' \link{parent.frame} of called LittleThumb functions.
#'
#' @param makedir
#' logical value, if \code{TRUE} non-existing folders are created automatically
#' when necessary.
#'
#' @param overload
#' logical value, if \code{TRUE} forces to load R objects from associated RDS
#' files even when these objects already exist in the R environment
#' (default = F, no).
#'
#' @param rebuild
#' logical value, if \code{TRUE} forces \code{MakeObj} to regenerate R objects
#' even when the associated RDS files already exist (default = F, no).
#'
#' @param cleanup
#' logical value controlling wheter \code{MakeObj} should remove additional
#' objects created while evaluating the provided expression (default = T, yes).
#'
#' @param remove
#' logical value controlling wheter \code{DeleteObj} should remove the R object
#' after deleting the RDS file (default = T, yes).
#'
#' @param messages
#' logical value enabling or disabling status messages from LittleThumb
#' functions (default = T, enabled).
#'
#' @param history
#' TODO: implementation & documentation
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
    if(any(is.na(names(cfg)))) {
      stop("missing global options")
    } else {
      names(cfg) <- gsub("^LittleThumb\\.", "", names(cfg))
    }
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
