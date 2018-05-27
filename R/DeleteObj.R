# =============================================================================.
#' Delete the RDS file associated to an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{SaveObj}
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
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = DeleteObj)

  if(! is.environment(envir)) envir <- parent.frame()

  f <- PathToRDS(obj.name, path, cfg$extension, relative)
  f.e <- file.exists(f)

  if(f.e) msg <- "delete" else msg <- "not found"
  if(messages) LittleThumb::StatusMessage(msg, obj.name, f)
  if(f.e) r <- file.remove(f)

  if(remove) {
    if(messages) LittleThumb::StatusMessage("remove", obj.name)
    r <- rm(list = obj.name, pos = envir, ...)
  }

}
