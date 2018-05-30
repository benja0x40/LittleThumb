# =============================================================================.
#' Load an R object from its associated RDS file
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{MakeObj},
#'   \link{SaveObj},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @example exec/examples/Basics.R
#' @inheritParams SaveObj
#'
#' @param obj
#' symbol corresponding to an R object previously made by \link{MakeObj}
#' or saved using \link{SaveObj}.
#'
#' @param reload
#' logical. When the R object to be loaded is already defined in the target
#' environment LoadObj can avoid (reload = FALSE) or force (reload = TRUE) the
#' reloading of this object.
#'
#' @param ...
#' optional arguments passed to the \link{readRDS} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
LoadObj <- function(
  obj, path = NULL, name = NULL, relative = NULL, messages = NULL,
  parent = NULL, parent.name = NULL, reload = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  if(is.null(parent.name) & ! missing(parent)) {
    parent.name <- deparse(substitute(parent))
  }

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = LoadObj)

  if(! is.environment(parent)) parent <- parent.frame()

  f <- PathToRDS(obj.name, path, relative)
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
}
