# =============================================================================.
#' Delete the RDS file associated to an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{SaveObj}
# -----------------------------------------------------------------------------.
#' @example exec/examples/Basics.R
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
  obj, path = NULL, name = NULL, relative = NULL, messages = NULL,
  parent = NULL, parent.name = NULL, remove = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  if(is.null(parent.name) & ! missing(parent)) {
    parent.name <- deparse(substitute(parent))
  }

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = DeleteObj)

  if(! is.environment(parent)) parent <- parent.frame()

  f <- PathToRDS(obj.name, path, relative)
  f.e <- file.exists(f)

  if(f.e) msg <- "delete" else msg <- "not found"
  if(messages) LittleThumb::StatusMessage(msg, obj.name, f)
  if(f.e) r <- file.remove(f)

  if(remove) {
    if(messages) LittleThumb::StatusMessage("remove", obj.name)
    r <- rm(list = obj.name, pos = parent, ...)
  }

}
