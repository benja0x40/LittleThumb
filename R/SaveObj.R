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
#' @param ...
#' optional arguments passed to the \link{saveRDS} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
SaveObj <- function(
  obj, path = NULL, name = NULL, parent = NULL, parent.name = NULL,
  relative = NULL, embedded = NULL, messages = NULL, makedir = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  prn.name <- parent.name
  if(is.null(prn.name)) prn.name <- deparse(substitute(parent))

  if(! IsKnowObject(obj.name)) RegisterObject(obj.name)
  if(IsKnowObject(prn.name)) SetParent(obj.name, prn.name)
  # if(is.null(parent) & ! is.null(parent.name)) parent <- get(prn.name)

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = SaveObj)

  if(! is.environment(parent)) parent <- parent.frame()

  f <- PathToRDS(obj.name, path, relative, embedded)
  if(! file.exists(f)) msg <- "save" else msg = "overwrite"

  o.e <- exists(x = obj.name, where = parent)
  if(! o.e) stop("object does not exist ", obj.name)

  d <- dirname(f)
  if(makedir & d != "" & ! file.exists(d)) {
    if(messages) LittleThumb::StatusMessage("create", d)
    LittleThumb::MkDir(d)
  }

  if(messages) LittleThumb::StatusMessage(msg, obj.name, f)
  saveRDS(parent[[obj.name]], f, ...)

  invisible(f)
}
