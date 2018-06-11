# =============================================================================.
#' Delete the RDS file associated to an R object
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
#' \code{parent}, \code{relative}, \code{embedded}, \code{remove}
#' and \code{messages}
#'
#' @param ...
#' optional arguments passed to the \link{rm} function.
# -----------------------------------------------------------------------------.
#' @export
DeleteObj <- function(
  obj, path = NULL, name = NULL, parent = NULL, parent.name = NULL,
  relative = NULL, embedded = NULL, messages = NULL, remove = NULL,
  origin = parent.frame(), ...
) {

  # Resolve arguments and automation options
  arg <- match.call()
  arg <- ManageObjectAndParentArgs(arg)
  opt <- LittleThumb()
  DefaultArgs(opt, ignore = c("obj", "name", "..."), from = DeleteObj)

  obj.name <- eval(arg$name, envir = origin)
  prn.name <- eval(arg$parent.name, envir = origin)
  parent   <- eval(arg$parent, envir = origin)

  f <- PathToRDS(obj.name, path, relative, embedded)
  f.e <- file.exists(f)

  if(f.e) msg <- "delete" else msg <- "not found"
  if(messages) LittleThumb::StatusMessage(msg, obj.name, f)
  if(f.e) r <- file.remove(f)

  o.e <- ObjectExists(obj.name, prn.name, parent, origin)
  if(remove & o.e) {
    if(messages) LittleThumb::StatusMessage("remove", obj.name)
    RemoveObject(obj.name, prn.name, parent, origin)
  }

  if(! o.e) warning("object does not exist")
  if(! f.e) warning("RDS file does not exist")

  ForgetObject(obj.name)
}
