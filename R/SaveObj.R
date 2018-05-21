# =============================================================================.
#' Save an R object as RData with semi-automated path and file name
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{AvailableObj}
# -----------------------------------------------------------------------------.
#' @param obj
#' an R object to be saved as RData.
#'
#' @param path
#' specific location used to save the RData.
#' When none is specified this location is defined by the path option accessible
#' via \link{LittleThumb}, which by default is the current working directory.
#'
#' @param name
#' optional file name. When omitted this name is automatically set to the
#' name of the R object being passed as argument.
#'
#' @param env
#' the \link{environment} containing the R object to be saved
#' (default = .GlobalEnv).
#'
#' @param ...
#' optional parameters passed to the \link{saveRDS} function.
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
    message("[creating] ", d)
    LittleThumb::mkdir(d)
  }

  message(msg, " ", f)
  saveRDS(env[[obj.name]], f, ...)

  f
}
