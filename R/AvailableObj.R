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
AvailableObj <- function(obj, path = NULL, name = NULL) {

  cfg <- LittleThumb::lt_cfg() # LittleThumb options

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  f <- MakePath(path, obj.name, ext = cfg$extension)

  file.exists(f)

}
