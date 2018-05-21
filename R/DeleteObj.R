# =============================================================================.
#' DeleteObj
# -----------------------------------------------------------------------------.
#' @export
DeleteObj <- function(obj, path = NULL, name = NULL, ...) {

  cfg <- LittleThumb::lt_cfg() # LittleThumb options

  obj.name <- name
  if(is.null(obj.name)) obj.name <- deparse(substitute(obj))

  f <- MakePath(path, obj.name, ext = cfg$extension)
  f.e <- file.exists(f)

  if(f.e) msg <- "[deleting]" else msg <- "[not found]"
  message(msg, " ", f)
  if(f.e) res <- file.remove(f)

}
