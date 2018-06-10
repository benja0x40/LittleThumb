# =============================================================================.
#' Reassign an R object from its parent environment to another one
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{LittleThumb}
# -----------------------------------------------------------------------------.
#' @inheritParams LittleThumb
#' @inheritParams SaveObj
#'
#' @param from
#' source environment, where the object is currently located.
#'
#' @param to
#' destination environment.
#'
#' @param remove
#' logical value controlling wheter the object should removed from the source
#' environment (default = \code{TRUE}, yes).
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
AssignObj <- function(
  obj, name = NULL, from = NULL, to = NULL, remove = TRUE,
  origin = parent.frame()
) {

  if(missing(obj)) src_name <- name else src_name <- deparse(substitute(obj))
  if(is.null(src_name)) stop("missing 'obj' or 'name' argument")
  dst_name <- name
  if(is.null(dst_name)) dst_name <- src_name

  f_name <- deparse(substitute(from))
  t_name <- deparse(substitute(to))
  if(is.character(from)) f_name <- from
  if(is.character(to))   t_name <- to
  if(is.null(from)) from <- origin
  if(is.null(to)) to <- origin
  f_e <- is.environment(from)
  t_e <- is.environment(to)

  if(! (f_e)) if(! exists(f_name, where = origin)) stop("'from' error")
  if(! (t_e)) if(! exists(t_name, where = origin)) stop("'to' error")

  if(src_name != dst_name | ! identical(from, to)) {

    if(t_e) to[[dst_name]] <- from[[src_name]]
    else origin[[t_name]][[dst_name]] <- from[[src_name]]

    if(remove) {
      if(f_e) rm(list = src_name, pos = from)
      else origin[[f_name]][[src_name]] <- NULL
    }
  }

}
