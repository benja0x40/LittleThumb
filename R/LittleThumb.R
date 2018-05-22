# =============================================================================.
#' DefaultArgs ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DefaultArgs <- function(f, cfg, ignore = NULL, env = NULL) {

  if(is.null(env)) {
    env <- parent.frame()
    lst <- setdiff(formalArgs(f), ignore)
  } else {
    lst <- setdiff(names(cfg), ignore)
  }

  for(a in lst) {
    if(is.null(env[[a]]) & ! is.null(cfg[[a]])) {
      env[[a]] <- cfg[[a]]
    }
  }
}

# =============================================================================.
#' LogicalArg ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
LogicalArg <- function(x, a) {

  r <- vector("list", length(a))

  if(! is.null(names(a))) {
    names(r) <- names(a)
    r[] <- as.logical(a)
  } else if(is.character(a)) {
    names(r) <- a
    r[] <- T
  } else {
    names(r) <- x
    r[] <- as.logical(a)
  }

  r <- ifelse(is.null(r[[x]]), F, r[[x]])

  r
}

# =============================================================================.
#' DefaultOptions ** RESERVED FOR INTERNAL USE **
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
    remove   = F,

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
#'   \link{SaveObj},
#'   \link{LoadObj}
# -----------------------------------------------------------------------------.
#' @param path
#' default folder used to save R objects.
#'
#' @param relative
#' logical value. When \code{TRUE} the default folder defines the root path for
#' all read/write operations automated by LittleThumb, meaning that any path
#' specified when calling \link{MakeObj}, \link{SaveObj} or \link{LoadObj}
#' will represent a sub path of the default folder.
#'
#' @param extension
#' RDS file extension (default = ".rds").
#'
#' @param envir
#' environment containing R objects for \link{MakeObj}, \link{SaveObj} and
#' \link{LoadObj}. With the default value (\code{NA}), this environment is
#' the parent.frame of called LittleThumb functions.
#'
#' @param makedir
#' logical value, if \code{TRUE} non-existing folders are created automatically
#' when necessary.
#'
#' @param rebuild
#' logical value, if \code{TRUE} forces \link{MakeObj} to regenerate R objects
#' even if when the associated RDS files already exist.
#'
#' @param overload
#' logical value, if \code{TRUE} forces \link{MakeObj} and \link{LoadObj} to
#' load R objects from associated RDS files even when these objects already
#' exist in the R environment.
#'
#' @param messages
#' logical value enabling or disabling status messages from LittleThumb
#' functions (default = T, yes).
#'
#' @param history
#' file name.
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
#' ResetOptions ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ResetOptions <- function() {
  do.call(LittleThumb, LittleThumb::DefaultOptions())
}
