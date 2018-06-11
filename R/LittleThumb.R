# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' List of global options for the LittleThumb package
# -----------------------------------------------------------------------------.
#' @import methods
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DefaultOptions <- function() {
  list(
    # Experimental
    # registry  = "LittleThumb", # TODO: implementation
    # context   = NA,            # TODO: implementation

    # Path generation (SaveObj, LoadObj, DeleteObj, AvailableObj)
    rootpath  = "",            # PathToRDS
    path      = NA,            #
    extension = ".rds",        #
    relative  = TRUE,          # + PathToRDS
    embedded  = TRUE,          # + PathToRDS

    # Evaluation (SaveObj, LoadObj, DeleteObj, MakeObj)
    parent = NA,

    # Behavior
    makedir  = TRUE,           # SaveObj <= MakeObj
    reload   = FALSE,          # LoadObj <= MakeObj
    rebuild  = FALSE,          # MakeObj
    cleanup  = TRUE,           # MakeObj
    remove   = TRUE,           # DeleteObj

    # Status
    messages = TRUE            # SaveObj, LoadObj, DeleteObj, MakeObj
  )
}

# =============================================================================.
#' Automation options
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj}
# -----------------------------------------------------------------------------.
#' @description
#' This function sets the default value of arguments used by the main functions
#' of the LittleThumb package.
#'
#' @param rootpath
#' root location for RDS files. By default \code{rootpath} is the current
#' working directory (see \link{getwd}).
#'
#' @param path
#' folder location for RDS files. LittleThumb interprets this path either as
#' a sub-directory of the \code{rootpath} location, or as an independent
#' location (see \code{relative}). By default \code{path} is ignored
#' (\code{path = NA}).
#'
#' @param extension
#' \strong{DO NOT MODIFY}: RDS file extension (default = ".rds").
#'
#' @param relative
#' logical value controlling if the \code{path} option is interpreted as a
#' relative location, meaning as a sub-directory of the \code{rootpath}
#' location (default = \code{TRUE}, yes), or as an independent one.
#'
#' @param embedded
#' logical value controlling whether child objects should have their associated
#' RDS files organized into sub-directories corresponding to parent objects
#' (default = \code{TRUE}, yes).
#'
#' @param parent
#' \link{environment} where the R object should be located.
#' By default \code{parent} is the \link{parent.frame} of the function
#' being called.
#'
#' @param makedir
#' logical value controlling if non-existing folders should be created
#' automatically when necessary (default = \code{TRUE}, yes).
#'
#' @param reload
#' logical value forcing to load R objects from associated RDS files
#' even when these objects already exist in their parent environment
#' (default = \code{FALSE}, no).
#'
#' @param rebuild
#' logical value forcing \code{MakeObj} to regenerate R objects even when
#' the associated RDS files already exist at the expected locations
#' (default = \code{FALSE}, no).
#'
#' @param cleanup
#' logical value controlling wheter \code{MakeObj} should remove from the
#' parent environment any additional object created by the provided
#' expression during its evaluation (default = \code{TRUE}, yes).
#'
#' @param remove
#' logical value controlling wheter \code{DeleteObj} should remove the R object
#' from its parent environment after deleting the associated RDS file
#' (default = \code{TRUE}, yes).
#'
#' @param messages
#' logical value enabling or disabling status messages from LittleThumb
#' functions (default = \code{TRUE}, enabled).
# -----------------------------------------------------------------------------.
#' @export
LittleThumb <- function(

  rootpath  = NULL,
  path      = NULL,
  extension = NULL,
  relative  = NULL,
  embedded  = NULL,

  parent    = NULL,

  makedir   = NULL,
  reload    = NULL,
  rebuild   = NULL,
  cleanup   = NULL,
  remove    = NULL,

  messages  = NULL
) {

  opt <- names(LittleThumb::DefaultOptions())

  cfg <- match.call()
  cfg[1] <- call("list")
  cfg <- eval(cfg)

  cfg <- cfg[names(cfg) %in% opt]
  cfg <- cfg[! vapply(cfg, is.null, logical(1))]

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

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
StatusMessage <- function(status, name = NULL, file = NULL) {
  msg <- paste0("[LittleThumb] ", status)
  if(! is.null(name)) msg <- paste0(msg, " | ", name)
  if(! is.null(file)) msg <- paste0(msg, " = ", file)
  message(msg)
}
