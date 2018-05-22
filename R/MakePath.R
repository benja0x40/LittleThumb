# =============================================================================.
#' Concatenate several strings to form a filesystem path
# -----------------------------------------------------------------------------.
#' @param ...
#' list of character strings forming a file path.
#'
#' @param ext
#' file name extension (default = "", none).
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
  DefaultArgs(MakePath, cfg, ignore = c("ext", "..."))

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
