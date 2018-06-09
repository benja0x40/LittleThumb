# =============================================================================.
#' Load an R object from its associated RDS file
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
#' \code{parent}, \code{relative}, \code{embedded}, \code{reload}
#' and \code{messages}
#'
#' @param ...
#' optional arguments passed to the \link{readRDS} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
LoadObj <- function(
  obj, path = NULL, name = NULL, parent = NULL, parent.name = NULL,
  relative = NULL, embedded = NULL, messages = NULL, reload = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  prn.name <- parent.name
  if(is.null(prn.name)) prn.name <- deparse(substitute(parent))

  if(! IsKnowObject(obj.name)) RegisterObject(obj.name)
  if(IsKnowObject(prn.name)) SetParent(obj.name, prn.name)
  # if(is.null(parent) & ! is.null(parent.name)) parent <- get(prn.name)

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = LoadObj)

  if(! is.environment(parent)) parent <- parent.frame()

  f <- PathToRDS(obj.name, path, relative, embedded)
  f.e <- file.exists(f)
  if(! f.e) stop("file not found ", f)

  reload <- LogicalArg(obj.name, reload)

  o.e <- exists(x = obj.name, where = parent)
  if(o.e) msg <- "reload" else msg <- "load"
  if(o.e & ! reload) msg <- "bypass"

  if(messages) LittleThumb::StatusMessage(msg, obj.name, f)
  if(f.e & (reload | ! o.e)) {
    r <- readRDS(f, ...)
    AssignObj(r, name = obj.name, to = parent)
  }

  invisible(f)
}
