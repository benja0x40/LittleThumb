
# HIDDEN #######################################################################

# =============================================================================.
#' Reassign object to a different environment
# -----------------------------------------------------------------------------.
#' @seealso
#' \link{LoadObj},
#' \link{SaveObj},
#' \link{AvailableObj},
#' \link{assign},
#' \link{environment}
# -----------------------------------------------------------------------------.
#' @param obj
#' an R object
#'
#' @param pos
#' the target \link{environment} where the object should be reassigned.
#' See \link{assign} for documentation on the different ways to specify
#' environments with the \code{pos} paramter.
#'
#' @param src
#' the source \link{environment} where the object is currently located
#' (default = .GlobalEnv).
#' See \link{assign} for documentation on the different ways to specify
#' environments with the \code{pos} paramter.
#'
#' @param keep
#' logical indicating if the original object should be removed or preserved
#' at its current location (defautl = F, remove).
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
reassign <- function(obj, pos, src = .GlobalEnv, keep = F) {
  obj.name <- deparse(substitute(obj))
  assign(obj.name, obj, pos = pos)
  if(! keep) {
    rm(list = obj.name, pos = src)
  }
}

