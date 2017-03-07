# Workspace ####################################################################

# =============================================================================.
# Add {name, path} to LTE$workspaces register
# -----------------------------------------------------------------------------.
define_workspace <- function(name, path) {

  if(! .lte_is_loaded.()) openLittleThumb()
  LTE <- .lte_env.()

  chk <- check_class(name, "character", error = T)
  chk <- check_class(path, "character", error = T)

  path <- suppressWarnings(normalizePath(path))

  chk <- is_registered(LTE$workspaces, k = 1, x = name)
  if(chk) stop("duplicate workspace name ", name)

  chk <- is_registered(LTE$workspaces, k = 2, x = path)
  if(chk) stop("duplicate workspace path ", path)

  x <- list(name = name, path = path, is_created = F, is_active = F)
  LTE$workspaces <- rbind(LTE$workspaces, x, stringsAsFactors = F)

  .lte_save.()
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
list_workspaces <- function(detailed = F) {

  if(! .lte_is_loaded.()) openLittleThumb()
  LTE <- .lte_env.()


  lst <- LTE$workspaces
  if(nrow(lst) > 0 & ! detailed) lst <- lst$name

  lst
}
# =============================================================================.
# Create workspace folders required by LittleThumb
# -----------------------------------------------------------------------------.
create_workspace <- function(name, path = NULL) {

  if(! .lte_is_loaded.()) openLittleThumb()
  LTE <- .lte_env.()

  if(! is.null(path)) define_workspace(name, path)
  path <- register_value(LTE$workspaces, k = 1, x = name, v = 2)

  # Create workspace folders
  create_paths(LTE$config, is_dir == T  & level == "workspace", root = path)

  idx <- which(td_selector(LTE$workspaces, name == name))
  LTE$workspaces[idx, "is_created"] <- T
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
workspace <- function() {

  if(! .lte_is_loaded.()) openLittleThumb()
  LTE <- .lte_env.()

  td_selector(LTE$workspaces, is_active == T, v = "name")
}
