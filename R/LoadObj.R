# =============================================================================.
#' Load an R object from RData with semi-automated path and file name
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{SaveObj},
#'   \link{AvailableObj}
# -----------------------------------------------------------------------------.
#' @param obj
#' an R object previously generated with \link{MakeObj} or saved using
#' \link{SaveObj}.
#'
#' @param path
#' specific location where to look for the RData.
#' When none is specified this location is defined by the path option accessible
#' via \link{LittleThumb}, which by default is the current working directory.
#'
#' @param name
#' optional file name. When omitted this name is automatically set to the
#' name of the R object being passed as argument.
#'
#' @param env
#' the \link{environment} where the R object should be loaded
#' (default = .GlobalEnv).
#'
#' @param overload
#' logical. When the R object to be loaded is already defined in the target
#' environment LoadObj can avoid (overload = F) or force (overload = T) the
#' reloading of this object.
#'
#' @param ...
#' optional parameters passed to the \link{readRDS} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @examples
#' # R objects are automatically saved in the current working directory
#' a <- rep("a", 10)
#' SaveObj(a)
#' rm(a)
#' LoadObj(a)
#' print(a)
#' file.remove("a.rdata") # delete the file generated for this example
#'
#' # R objects can be saved and loaded from any path
#' b <- rep("b", 10)
#' SaveObj(b, path = "MyRData") # path is automatically created if necessary
#' b <- NULL
#' LoadObj(b, path = "MyRData")
#'
#' # Not yet defined R objects may be created only when no saved version exists
#' if(! LoadObj(a, "MyRData")) {
#'   a <- 0
#'   SaveObj(a, "MyRData")
#' }
#' print(a)
#'
#' # Saved object can be updated by overwriting
#' a <- 1
#' SaveObj(a, path = "MyRData")
#' rm(a)
#' LoadObj(a)
#' print(a)
# -----------------------------------------------------------------------------.
#' @export
LoadObj <- function(
  obj, path = NULL, name = NULL, env = NULL, overload = NULL, ...
) {

  cfg <- LittleThumb::lt_cfg() # LittleThumb options
  if(is.null(env)) env <- cfg$environment
  if(! is.environment(env)) env <- parent.frame()

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))
  o.e <- exists(x = obj.name, where = env)

  f <- MakePath(path, obj.name, ext = cfg$extension)
  f.e <- file.exists(f)
  if(! f.e) stop("file not found ", f)

  if(is.null(overload)) overload <- cfg$overload

  if(o.e) msg <- "[overloading]" else msg <- "[loading]"
  if(o.e & ! overload) msg <- "[passing]"

  message(msg, " ", f)
  if(f.e & (overload | ! o.e)) {
    res <- assign(obj.name, readRDS(f, ...), pos = env)
  }
}
