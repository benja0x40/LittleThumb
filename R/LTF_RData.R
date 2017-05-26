# =============================================================================.
#' default path for saving R objects
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{saveObj},
#'   \link{loadObj}
# -----------------------------------------------------------------------------.
#' @param path
#' base folder for saving R objects. By default, path to this folder is defined
#' by the value of \code{.Cfg$rdata.path} if such configuration variable exists.
#' Otherwise it is the current working directory when none is specified.
#'
#' @param name
#' a file name (default = none).
#'
# -----------------------------------------------------------------------------.
#' @return path
# -----------------------------------------------------------------------------.
rdataPath <- function(path = NULL, name = NULL) {

  if(is.null(path)) {
    # Use predefined rdata path when it exists
    if(exists(".Cfg", where = .GlobalEnv)) {
      if(! is.null(.Cfg$rdata.path)) {
        path <- as.character(.Cfg$rdata.path)
      }
    } else {
      path = ""
    }
  }

  # Append a file directory delimiter if ommited
  if(path!="" & ! grepl("/$", path, perl=T)) {
    path <- paste(path, "/", sep="")
  }

  # Build a path for a file (optional)
  if(! is.null(name)) {
    path <- paste(path, name, ".rdata", sep="")
  }

  path
}
# =============================================================================.
#' save an R object as RData with automated path and file name
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{loadObj},
#'   \link{rdataPath},
#'   \link{saveRDS},
#'   \link{save.image},
#'   \link{reassign}
# -----------------------------------------------------------------------------.
#' @param obj
#' an R object to be saved as RData.
#'
#' @inheritParams rdataPath
#'
#' @param ...
#' optional parameters passed to the \link{saveRDS} function.
# -----------------------------------------------------------------------------.
#' @return NULL
# -----------------------------------------------------------------------------.
#' @examples
#' # R objects are automatically saved in the current working directory
#' a <- rep("a", 10)
#' saveObj(a)
#' rm(a)
#' loadObj(a)
#' print(a)
#' file.remove("a.rdata") # delete the file generated for this example
#'
#' # R objects can be saved and loaded from any path
#' b <- rep("b", 10)
#' saveObj(b, path = "MyRData") # path is automatically created if necessary
#' b <- NULL
#' loadObj(b, path = "MyRData")
#'
#' # Not yet defined R objects may be created only when no saved version exists
#' if(! loadObj(a, "MyRData")) {
#'   a <- 0
#'   saveObj(a, "MyRData")
#' }
#' print(a)
#'
#' # Saved object can be updated by overwriting
#' a <- 1
#' saveObj(a, path = "MyRData")
#' rm(a)
#' loadObj(a)
#' print(a)
# -----------------------------------------------------------------------------.
#' @export
saveObj <- function(obj, path = NULL, name = NULL, ...) {
  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))
  fpath <- rdataPath(path, obj.name)
  path <- dirname(fpath)
  if(path!="" & ! file.exists(path)) {
    message("[creating] ", path)
    system(paste("mkdir -p", path))
  }
  if(! file.exists(fpath)) {
    message("[saving] ", fpath)
  } else {
    message("[updating] ", fpath)
  }
  saveRDS(obj, fpath, ...)
}
# =============================================================================.
#' load an R object from RData with automated path and file name
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{saveObj},
#'   \link{rdataPath},
#'   \link{readRDS},
#'   \link{reassign}
# -----------------------------------------------------------------------------.
#' @param obj
#' an R object previously saved using \link{saveObj}.
#'
#' @inheritParams rdataPath
#'
#' @param need
#' logical, when \code{TRUE} loadObj raises an error if object loading fails
#' (default = F).
#'
#' @param pos
#' the target \link{environment} where the object should be loaded
#' (default = .GlobalEnv).
#' See \link{assign} for documentation on the different ways to specify
#' environments with the \code{pos} paramter.
#'
#' @param ...
#' optional parameters passed to the \link{readRDS} function.
# -----------------------------------------------------------------------------.
#' @return logical
#' \code{TRUE} when object was loaded successfully, \code{FALSE} otherwise.
# -----------------------------------------------------------------------------.
#' @examples
#' # R objects are automatically saved in the current working directory
#' a <- rep("a", 10)
#' saveObj(a)
#' rm(a)
#' loadObj(a)
#' print(a)
#' file.remove("a.rdata") # delete the file generated for this example
#'
#' # R objects can be saved and loaded from any path
#' b <- rep("b", 10)
#' saveObj(b, path = "MyRData") # path is automatically created if necessary
#' b <- NULL
#' loadObj(b, path = "MyRData")
#'
#' # Not yet defined R objects may be created only when no saved version exists
#' if(! loadObj(a, "MyRData")) {
#'   a <- 0
#'   saveObj(a, "MyRData")
#' }
#' print(a)
#'
#' # Saved object can be updated by overwriting
#' a <- 1
#' saveObj(a, path = "MyRData")
#' rm(a)
#' loadObj(a)
#' print(a)
# -----------------------------------------------------------------------------.
#' @export
loadObj <- function(
  obj, path = NULL, name = NULL, need = F, pos = .GlobalEnv, overload = F, ...
) {
  need <- need # Force evaluation of need to avoid overloading issues
  obj.name <- name
  if(is.null(obj.name) & ! missing(obj)) {
    obj.name <- deparse(substitute(obj))
  }
  if(is.null(obj.name)) {
    stop("either obj or name must be specified")
  }
  fpath <- rdataPath(path, obj.name)
  chk <- exists(x = obj.name, where = pos)
  msg <- ifelse(chk, "[overloading] ", "[loading] ")
  res <- NULL
  if(file.exists(fpath)) {
    if(chk & ! overload) {
      message("[skipped] ", fpath)
      res <- T
    } else {
      message(msg, fpath)
      res <- assign(obj.name,readRDS(fpath, ...), pos = pos)
    }
  }
  if(need & is.null(res)) {
    stop("failed to load ", fpath)
  }
  ! is.null(res)
}
# -----------------------------------------------------------------------------.
#' @export
availableObj <- function(obj, path = NULL, name = NULL) {
  obj.name <- name
  if(is.null(obj.name) & ! missing(obj)) {
    obj.name <- deparse(substitute(obj))
  }
  if(is.null(obj.name)) {
    stop("either obj or name must be specified")
  }
  fpath <- rdataPath(path, obj.name)

  file.exists(fpath)
}
