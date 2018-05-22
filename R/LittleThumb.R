# =============================================================================.
#' lt_cfg
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
lt_cfg <- function() {

  lst <- c(
    "path", "root", "extension",
    "makedir", "rebuild", "overload", "messages",
    "environment"
  )
  r <- options()[paste0("LittleThumb.", lst)]
  names(r) <- gsub( "^LittleThumb.", "", names(r))

  r
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
#' @param root
#' logical value. When \code{TRUE} the default folder defines the root path for
#' all read/write operations automated by LittleThumb, meaning that any path
#' specified when calling \link{MakeObj}, \link{SaveObj} or \link{LoadObj}
#' will represent a sub path of the default folder.
#'
#' @param extension
#' RDS file extension (default = ".rds").
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
#' @param environment
#' environment containing R objects for \link{MakeObj}, \link{SaveObj} and
#' \link{LoadObj}. With the default value (\code{NA}), this environment is
#' the parent.frame of called LittleThumb functions.
# -----------------------------------------------------------------------------.
#' @export
LittleThumb <- function(
  path = NULL, root = NULL, extension = NULL,
  makedir = NULL, rebuild = NULL, overload = NULL, messages = NULL,
  environment = NULL
) {

  # Path generation
  if(! is.null(path)) options(LittleThumb.path = path)
  if(! is.null(root)) options(LittleThumb.root = root)
  if(! is.null(extension)) options(LittleThumb.extension = extension)

  # Behavior
  if(! is.null(makedir))  options(LittleThumb.makedir  = makedir)
  if(! is.null(rebuild))  options(LittleThumb.rebuild  = rebuild)
  if(! is.null(overload)) options(LittleThumb.overload = overload)
  if(! is.null(messages)) options(LittleThumb.messages = messages)

  # Evaluation
  if(! is.null(environment)) options(LittleThumb.environment = environment)

  lst <- list(
    path, root, extension, makedir, rebuild, overload, environment
  )
  if(all(sapply(lst, is.null))) lt_cfg()
}
