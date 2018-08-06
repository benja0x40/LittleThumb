# =============================================================================.
#' Save the RDS file associated to an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{LittleThumb}
# -----------------------------------------------------------------------------.
#' @example exec/examples/Basics.R
#' @inheritParams LittleThumb
#'
#' @details
#' When unspecified, the value of the following argument(s) are determined
#' by the corresponding automation option(s) (see \link{LittleThumb}):
#'
#' \code{parent}, \code{relative}, \code{embedded}, \code{makedir}
#' and \code{messages}
#'
#' @param obj
#' an R object.
#'
#' @param name
#' \strong{RESERVED FOR INTERNAL USE}: optional name of the R object.
#' When omitted this name is automatically provided by the symbol being passed
#' as first argument using \link{substitute} and \link{deparse}.
#'
#' @param parent.name
#' optional name of the parent environment.
#'
#' @param origin
#' \strong{RESERVED FOR INTERNAL USE}: parent.frame of the initial function
#' call.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SaveObj <- function(
  obj, path = NULL, name = NULL, parent = NULL, parent.name = NULL,
  relative = NULL, embedded = NULL, messages = NULL, makedir = NULL,
  origin = parent.frame()
) {

  # Resolve arguments and automation options
  arg <- match.call()
  arg <- ManageObjectAndParentArgs(arg)
  opt <- LittleThumb()
  DefaultArgs(opt, ignore = c("obj", "name", "..."), from = SaveObj)

  obj.name <- eval(arg$name, envir = origin)
  prn.name <- eval(arg$parent.name, envir = origin)
  parent   <- eval(arg$parent, envir = origin)

  makedir <- LogicalArg(obj.name, makedir)

  f <- PathToRDS(obj.name, path, relative, embedded)
  if(! file.exists(f)) msg <- "save" else msg = "update"

  o.e <- ObjectExists(obj.name, prn.name, parent, origin)
  if(! o.e) stop("object does not exist ", obj.name)

  d <- dirname(f)
  if(makedir & d != "" & ! file.exists(d)) {
    if(messages) LittleThumb::StatusMessage("create", d)
    LittleThumb::MkDir(d)
  }

  if(messages) LittleThumb::StatusMessage(msg, obj.name, f, "=>")
  saveRDS(GetValue(obj.name, prn.name, parent, origin), f)

  invisible(f)
}
