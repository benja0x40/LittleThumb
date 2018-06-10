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
  obj, path = NULL, name = NULL, parent = NULL, parent.name = NULL,
  relative = NULL, embedded = NULL, origin = parent.frame()
) {

  # Resolve arguments and automation options
  arg <- match.call()
  arg <- ManageObjectAndParentArgs(arg)
  opt <- LittleThumb()
  DefaultArgs(opt, ignore = c("obj", "name", "..."), from = AvailableObj)

  obj.name <- eval(arg$name, envir = origin)

  f <- PathToRDS(obj.name, path, relative, embedded)

  file.exists(f)

}
