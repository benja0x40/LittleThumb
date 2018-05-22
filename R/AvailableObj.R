# =============================================================================.
#' Check if the RDS file associated to an R object is available
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LoadObj},
#'   \link{MakeObj},
#'   \link{SaveObj},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @example examples/Basics.R
#' @inheritParams LoadObj
#'
#' @return
#' \code{TRUE} when the RDS file exists, \code{FALSE} otherwise.
# -----------------------------------------------------------------------------.
#' @export
AvailableObj <- function(obj, path = NULL, name = NULL, relative = NULL) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  cfg <- LittleThumb() # Global options
  DefaultArgs(AvailableObj, cfg, ignore = c("obj", "path", "name", "..."))

  f <- MakePath(path, obj.name, ext = cfg$extension, relative = relative)

  file.exists(f)

}
