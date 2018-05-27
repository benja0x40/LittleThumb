# =============================================================================.
#' Save the RDS file associated to an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @example examples/Basics.R
#' @inheritParams LittleThumb
#'
#' @details
#' When unspecified, the value of the following arguments are determined by the
#' corresponding global options (see \link{LittleThumb}):
#'
#' \code{relative}, \code{envir}, \code{makedir} and \code{messages}
#'
#' @param obj
#' an R object to be saved as RDS file by the \link{saveRDS} function.
#'
#' @param path
#' directory where the RDS file should be located.
#'
#' @param name
#' \strong{RESERVED FOR INTERNAL USE}: optional name of the R object.
#' When omitted this name is automatically provided by the symbol being passed
#' as first argument.
#'
#' @param relative
#' logical value controlling whether the specified \code{path} should be
#' absolute or relative to the global \code{path} option accessible via the
#' \link{LittleThumb} function (default = T, yes).
#'
#' @param envir
#' \link{environment} where the R object should be located.
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
  DefaultArgs(cfg, ignore = c("obj", "name", "..."), from = SaveObj)

  if(! is.environment(envir)) envir <- parent.frame()

  f <- PathToRDS(obj.name, path, cfg$extension, relative)
  if(! file.exists(f)) msg <- "save" else msg = "overwrite"

  o.e <- exists(x = obj.name, where = envir)
  if(! o.e) stop("object does not exist ", obj.name)

  d <- dirname(f)
  if(makedir & d != "" & ! file.exists(d)) {
    if(messages) LittleThumb::StatusMessage("create", d)
    LittleThumb::MkDir(d)
  }

  if(messages) LittleThumb::StatusMessage(msg, obj.name, f)
  saveRDS(envir[[obj.name]], f, ...)

  f
}
