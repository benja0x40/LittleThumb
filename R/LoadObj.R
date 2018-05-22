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
  obj, path = NULL, name = NULL, relative = NULL, envir = NULL,
  overload = NULL, messages = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  cfg <- LittleThumb() # Global options
  DefaultArgs(LoadObj, cfg, ignore = c("obj", "path", "name", "..."))

  if(! is.environment(envir)) envir <- parent.frame()

  f <- MakePath(path, obj.name, ext = cfg$extension, relative = relative)
  f.e <- file.exists(f)
  if(! f.e) stop("file not found ", f)

  overload <- LogicalArg(obj.name, overload)

  o.e <- exists(x = obj.name, where = envir)
  if(o.e) msg <- "[overloading]" else msg <- "[loading]"
  if(o.e & ! overload) msg <- "[passing]"

  if(messages) message(msg, " ", f)
  if(f.e & (overload | ! o.e)) {
    res <- assign(obj.name, readRDS(f, ...), pos = envir)
  }
}
