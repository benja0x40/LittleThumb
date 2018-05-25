# =============================================================================.
#' Reassign an R object from one environment to another
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @param obj
#' an R symbol.
#'
#' @param name
#' optional new name of the R object whithin its destination environment.
#'
#' @param from
#' source environment where the object is currently located.
#'
#' @param to
#' destination environment.
#'
#' @param remove
#' logical indicating if the original object should be removed at its current
#' location (default = T, yes).
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
AssignObj <- function(obj, name = NULL, from = NULL, to = NULL, remove = T) {

  src_name <- deparse(substitute(obj))
  dst_name <- name
  if(is.null(dst_name)) dst_name <- src_name

  f_name <- deparse(substitute(from))
  t_name <- deparse(substitute(to))

  env <- parent.frame()
  if(is.null(from)) from <- env
  if(is.null(to)) to <- env

  f_e <- is.environment(from)
  t_e <- is.environment(to)
  f_l <- is.list(from)
  t_l <- is.list(to)

  if(! (f_e | f_l)) stop("'from' must be a list or an environment")
  if(! (t_e | t_l)) stop("'to' must be a list or an environment")

  if(src_name != dst_name | ! identical(from, to)) {

    if(t_e) to[[dst_name]] <- from[[src_name]]
    if(t_l) env[[t_name]][[dst_name]] <- from[[src_name]]

    if(remove) {
      if(f_e) rm(list = src_name, pos = from)
      if(f_l) env[[f_name]][[src_name]] <- NULL
    }
  }

}