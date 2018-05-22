# INCLUDES #####################################################################

# =============================================================================.
#' Load an R object from its associated RDS file
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{SaveObj},
#'   \link{LittleThumb}
# -----------------------------------------------------------------------------.
#' @example examples/Basics.R
#' @inheritParams SaveObj
#'
#' @param obj
#' symbol corresponding to an R object previously made by \link{MakeObj}
#' or saved using \link{SaveObj}.
#'
#' @param overload
#' logical. When the R object to be loaded is already defined in the target
#' environment LoadObj can avoid (overload = F) or force (overload = T) the
#' reloading of this object.
#'
#' @param ...
#' optional arguments passed to the \link{readRDS} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
LoadObj <- function(
  obj, path = NULL, name = NULL, env = NULL, overload = NULL, ...
) {

  cfg <- LittleThumb::lt_cfg() # LittleThumb options
  if(is.null(env)) env <- cfg$environment
  if(! is.environment(env)) env <- parent.frame()

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))
  o.e <- exists(x = obj.name, where = env)

  f <- MakePath(path, obj.name, ext = cfg$extension)
  f.e <- file.exists(f)
  if(! f.e) stop("file not found ", f)

  if(is.null(overload)) overload <- cfg$overload

  if(o.e) msg <- "[overloading]" else msg <- "[loading]"
  if(o.e & ! overload) msg <- "[passing]"

  if(cfg$messages) message(msg, " ", f)
  if(f.e & (overload | ! o.e)) {
    res <- assign(obj.name, readRDS(f, ...), pos = env)
  }
}
