# INCLUDES #####################################################################

# =============================================================================.
#' Delete the RDS file associated to an R object
# -----------------------------------------------------------------------------.
#' @seealso
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
  obj, path = NULL, name = NULL, env = NULL, remove = F, ...
) {

  cfg <- LittleThumb::lt_cfg() # LittleThumb options
  if(is.null(env)) env <- cfg$environment
  if(! is.environment(env)) env <- parent.frame()

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  f <- MakePath(path, obj.name, ext = cfg$extension)
  f.e <- file.exists(f)

  if(f.e) msg <- "[deleting]" else msg <- "[not found]"
  if(cfg$messages) message(msg, " ", f)
  if(f.e) r <- file.remove(f)

  if(remove) {
    r <- rm(list = obj.name, pos = env, ...)
    if(cfg$messages) message("[removing] ", obj.name)
  }

}
