# =============================================================================.
#' Delete the RDS file associated to an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{SaveObj},
#'   \link{AvailableObj}
# -----------------------------------------------------------------------------.
#' @example examples/Basics.R
#' @inheritParams LoadObj
#'
#' @param remove
#' logical.
#'
#' @param ...
#' optional arguments passed to the \link{rm} function.
# -----------------------------------------------------------------------------.
#' @export
DeleteObj <- function(
  obj, path = NULL, name = NULL, relative = NULL, envir = NULL,
  remove = NULL, messages = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "path", "name", "..."), fun = DeleteObj)

  if(! is.environment(envir)) envir <- parent.frame()

  f <- MakePath(path, obj.name, ext = cfg$extension, relative = relative)
  f.e <- file.exists(f)

  if(f.e) msg <- "[deleting]" else msg <- "[not found]"
  if(messages) message(msg, " ", f)
  if(f.e) r <- file.remove(f)

  if(remove) {
    if(messages) message("[removing] ", obj.name)
    r <- rm(list = obj.name, pos = envir, ...)
  }

}
