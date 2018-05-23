# =============================================================================.
#' Short alias of function MakePath
# -----------------------------------------------------------------------------.
#' @description
#' \code{MkPath} is a short alias of function \link{MakePath}.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MkPath <- function(...) { MakePath(...) }

# =============================================================================.
#' Concatenate several strings to form a filesystem path
# -----------------------------------------------------------------------------.
#' @description
#' \link{MkPath} is a short alias of function \code{MakePath}.
#'
#' @param ...
#' list of character strings forming a file path.
#'
#' @param ext
#' file name extension (default = "", none).
#'
#' @param relative
#' TODO: documentation
#'
#' @return
#' \code{MakePath} returns a \code{character} string.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MakePath <- function(..., ext = NULL, relative = NULL) {

  path <- list(...)
  path <- path[! sapply(path, is.null)]

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, ignore = c("ext", "..."), fun = MakePath)

  if(relative & ! cfg$path == "") path <- c(cfg$path, path)

  if(length(path) > 0) {
    path <- do.call(file.path, path)
    path <- gsub("[/]+", "/", path)
  } else {
    path <- cfg$path
  }

  if(! is.null(ext)) path = paste0(path, ext)

  path
}
