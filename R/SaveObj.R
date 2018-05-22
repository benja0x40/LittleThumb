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
#' \strong{reserved for internal use}: optional name of the R object.
#' When omitted this name is automatically provided by the symbol being passed
#' as first argument.
#'
#' @param env
#' \link{environment} where the R object should be located.
#' When none is specified this environment is defined by the \code{environment}
#' option accessible via the \link{LittleThumb} function.
#'
#' @param ...
#' optional arguments passed to the \link{saveRDS} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @export
SaveObj <- function(obj, path = NULL, name = NULL, env = NULL, ...) {

  cfg <- LittleThumb::lt_cfg() # LittleThumb options
  if(is.null(env)) env <- cfg$environment
  if(! is.environment(env)) env <- parent.frame()

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  f <- MakePath(path, obj.name, ext = cfg$extension)
  if(! file.exists(f)) msg <- "[saving]" else msg = "[overwriting]"

  d <- dirname(f)
  if(cfg$makedir & d != "" & ! file.exists(d)) {
    if(cfg$messages) message("[creating] ", d)
    LittleThumb::mkdir(d)
  }

  if(cfg$messages) message(msg, " ", f)
  saveRDS(env[[obj.name]], f, ...)

  f
}
