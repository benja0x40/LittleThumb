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
#' @return NULL
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
LoadObj <- function(
  obj, path = NULL, name = NULL, parent = NULL, parent.name = NULL,
  relative = NULL, embedded = NULL, messages = NULL, reload = NULL,
  origin = parent.frame()
) {

  # Resolve arguments and automation options
  arg <- match.call()
  arg <- ManageObjectAndParentArgs(arg)
  opt <- LittleThumb()
  DefaultArgs(opt, ignore = c("obj", "name", "..."), from = LoadObj)

  obj.name <- eval(arg$name, envir = origin)
  prn.name <- eval(arg$parent.name, envir = origin)
  parent   <- eval(arg$parent, envir = origin)

  reload <- LogicalArg(obj.name, reload)

  f <- PathToRDS(obj.name, path, relative, embedded)
  f.e <- file.exists(f)
  if(! f.e) stop("file not found ", f)

  sep <- "<="
  o.e <- ObjectExists(obj.name, prn.name, parent, origin)
  if(o.e) msg <- "reload" else msg <- "load"
  if(o.e & ! reload) {
    msg <- "bypass"
    sep <- NULL
  }

  if(messages) LittleThumb::StatusMessage(msg, obj.name, f, sep)
  if(f.e & (reload | ! o.e)) {
    SetValue(obj.name, prn.name, parent, origin, value = readRDS(f))
  }

  invisible(f)
}
