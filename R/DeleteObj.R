# =============================================================================.
#' Delete the RDS file associated to an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{LittleThumb}
# -----------------------------------------------------------------------------.
#' @example exec/examples/Basics.R
#' @inheritParams LittleThumb
#' @inheritParams SaveObj
#'
#' @details
#' When unspecified, the value of the following argument(s) are determined
#' by the corresponding automation option(s) (see \link{LittleThumb}):
#'
#' \code{parent}, \code{relative}, \code{embedded}, \code{remove}
#' and \code{messages}
#'
#' @param ...
#' optional arguments passed to the \link{rm} function.
# -----------------------------------------------------------------------------.
#' @export
DeleteObj <- function(
  obj, path = NULL, name = NULL, parent = NULL, parent.name = NULL,
  relative = NULL, embedded = NULL, messages = NULL, remove = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  prn.name <- parent.name
  if(is.null(prn.name)) prn.name <- deparse(substitute(parent))

  if(IsKnowObject(prn.name)) SetParent(obj.name, prn.name)
  # if(is.null(parent) & ! is.null(parent.name)) parent <- get(prn.name)

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = DeleteObj)

  if(! is.environment(parent)) parent <- parent.frame()

  f <- PathToRDS(obj.name, path, relative, embedded)
  f.e <- file.exists(f)

  if(f.e) msg <- "delete" else msg <- "not found"
  if(messages) LittleThumb::StatusMessage(msg, obj.name, f)
  if(f.e) r <- file.remove(f)

  if(remove) {
    if(messages) LittleThumb::StatusMessage("remove", obj.name)
    r <- rm(list = obj.name, pos = parent, ...)
  }
}
