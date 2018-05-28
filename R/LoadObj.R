# =============================================================================.
#' Load an R object from its associated RDS file
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{MakeObj},
#'   \link{SaveObj},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @example examples/Basics.R
#' @inheritParams SaveObj
#'
#' @param obj
#' symbol corresponding to an R object previously made by \link{MakeObj}
#' or saved using \link{SaveObj}.
#'
#' @param reload
#' logical. When the R object to be loaded is already defined in the target
#' environment LoadObj can avoid (reload = F) or force (reload = T) the
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
  reload = NULL, messages = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = LoadObj)

  if(! is.environment(envir)) envir <- parent.frame()

  f <- PathToRDS(obj.name, path, cfg$extension, relative)
  f.e <- file.exists(f)
  if(! f.e) stop("file not found ", f)

  reload <- LogicalArg(obj.name, reload)

  o.e <- exists(x = obj.name, where = envir)
  if(o.e) msg <- "reload" else msg <- "load"
  if(o.e & ! reload) msg <- "bypass"

  if(messages) LittleThumb::StatusMessage(msg, obj.name, f)
  if(f.e & (reload | ! o.e)) {
    res <- assign(obj.name, readRDS(f, ...), pos = envir)
  }
}
