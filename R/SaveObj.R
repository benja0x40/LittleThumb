# =============================================================================.
#' Save the RDS file associated to an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{LittleThumb}
# -----------------------------------------------------------------------------.
#' @example examples/Basics.R
#'
#' @param obj
#' an R object to be saved as RDS file by the \link{saveRDS} function.
#'
#' @param path
#' directory where the RDS file should be located.
#' When none is specified this location is defined by the \code{path} option
#' accessible via the \link{LittleThumb} function.
#'
#' @param name
#' \strong{RESERVED FOR INTERNAL USE}: optional name of the R object.
#' When omitted this name is automatically provided by the symbol being passed
#' as first argument.
#'
#' @param envir
#' \link{environment} where the R object should be located.
#' When none is specified this environment is defined by the \code{envir}
#' option accessible via the \link{LittleThumb} function.
#'
#' @param ...
#' optional arguments passed to the \link{saveRDS} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
SaveObj <- function(
  obj, path = NULL, name = NULL, relative = NULL, envir = NULL,
  makedir = NULL, messages = NULL, ...
) {

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  cfg <- LittleThumb() # Global options
  DefaultArgs(SaveObj, cfg, ignore = c("obj", "path", "name", "..."))

  if(! is.environment(envir)) envir <- parent.frame()

  f <- MakePath(path, obj.name, ext = cfg$extension, relative = relative)
  if(! file.exists(f)) msg <- "[saving]" else msg = "[overwriting]"

  o.e <- exists(x = obj.name, where = envir)
  if(! o.e) stop("object does not exist ", obj.name)

  d <- dirname(f)
  if(makedir & d != "" & ! file.exists(d)) {
    if(messages) message("[creating] ", d)
    LittleThumb::mkdir(d)
  }

  if(messages) message(msg, " ", f)
  saveRDS(envir[[obj.name]], f, ...)

  f
}
