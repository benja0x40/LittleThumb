# =============================================================================.
#' Check if the RDS file associated to an R object is available
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
#' @return
#' \code{TRUE} when the RDS file exists, \code{FALSE} otherwise.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
AvailableObj <- function(obj, path = NULL, name = NULL, relative = NULL) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), fun = AvailableObj)

  f <- PathToRDS(obj.name, path, cfg$extension, relative)

  file.exists(f)

}
