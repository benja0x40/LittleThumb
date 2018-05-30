# =============================================================================.
#' Short alias of function MakeDir
# -----------------------------------------------------------------------------.
#' @description
#' \code{MkDir} is a short alias of the \link{MakeDir} function.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MkDir <- function(...) { MakeDir(...) }

# =============================================================================.
#' Create a destination path (Linux/macOS only)
# -----------------------------------------------------------------------------.
#' @description
#' \code{MakeDir} (short alias \link{MkDir}) creates a destination path
#' including upper level directories if necessary.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MakeDir <- function(x) {
  system(paste0('mkdir -p "', x, '"'))
}

# =============================================================================.
#' Short alias of function MakePath
# -----------------------------------------------------------------------------.
#' @description
#' \code{MkPath} is a short alias of the \link{MakePath} function.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MkPath <- function(...) { MakePath(...) }

# =============================================================================.
#' Construct a file path
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{MakeObj},
#'   \link{LoadObj},
#'   \link{SaveObj},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @description
#' \code{MakePath} (short alias \link{MkPath}) concatenates its arguments to
#' form corresponding filesystem paths, similarly to the base function
#' \link{file.path}.
#' The generated paths depend on LittleThumb's global options \code{path}
#' and \code{relative}.
#'
#' @param ...
#' list of character vectors to be assembled into file paths.
#'
#' @param ext
#' file name extension(s) (default = "", none).
#'
#' @param relative
#' logical value (see \link{LittleThumb}).
#'
#' @return
#' \code{MakePath} returns a \code{character} vector.
# -----------------------------------------------------------------------------.
#' @export
MakePath <- function(..., ext = NULL) { # , rootpath = NULL, relative = NULL

  path <- list(...)
  path <- path[! vapply(path, is.null, logical(1))]
  path <- path[! vapply(path, is.na, logical(1))]
  path <- path[! path == ""]

  # cfg <- LittleThumb() # Global options
  # DefaultArgs(cfg, ignore = c("ext", "..."), from = MakePath)

  if(length(path) < 1) stop("empty path")

  # if(relative & rootpath != "") path <- c(rootpath, path)

  path <- do.call(file.path, path)
  path <- gsub("[/]+", "/", path)

  if(! is.null(ext)) path <- paste0(path, ext)

  path
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Resolve path to the RDS file of an R object
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
PathToRDS <- function(name, path = NULL, relative = NULL) {

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("name", "path"), from = PathToRDS)

  path     <- NamedArg(name, path)
  relative <- NamedArg(name, relative)

  if(relative & cfg$rootpath != "") path <- c(cfg$rootpath, path)

  f <- c(path, name, list(ext = cfg$extension))
  f <- do.call(MakePath, f)

  f
}

