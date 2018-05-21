# =============================================================================.
#' Check if an R object is available as RData
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LoadObj},
#'   \link{MakeObj},
#'   \link{SaveObj}
# -----------------------------------------------------------------------------.
#' @inheritParams LoadObj
#'
#' @return
#' logical, \code{TRUE} when RData is available and \code{FALSE} otherwise.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
AvailableObj <- function(obj, path = NULL, name = NULL) {

  cfg <- LittleThumb::lt_cfg() # LittleThumb options

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  f <- MakePath(path, obj.name, ext = cfg$extension)

  file.exists(f)

}
