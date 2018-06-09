# =============================================================================.
#' Check if the RDS file associated to an R object is available
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
#' \code{relative} and \code{embedded}
#'
#' @return
#' \code{TRUE} when the RDS file exists, \code{FALSE} otherwise.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
AvailableObj <- function(
  obj, path = NULL, name = NULL, relative = NULL, embedded = NULL
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = AvailableObj)

  f <- PathToRDS(obj.name, path, relative, embedded)

  file.exists(f)

}
