# Workspace ####################################################################

# =============================================================================.
# Add {name, path} to LTE$workspaces register
# -----------------------------------------------------------------------------.
define_workspace <- function(name, path) {

  LTE <- .lte_env.()

  chk <- check_class(name, "character", error = T)
  chk <- check_class(path, "character", error = T)

  path <- suppressWarnings(normalizePath(path))

  chk <- is_registered(LTE$workspaces, k = 1, x = name)
  if(chk) stop("duplicate workspace name ", name)

  chk <- is_registered(LTE$workspaces, k = 2, x = path)
  if(chk) stop("duplicate workspace path ", path)

  x <- list(name = name, path = path, is_created = F, is_opened = F)
  LTE$workspaces <- rbind(LTE$workspaces, x, stringsAsFactors = F)

  .lte_save.()
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
list_workspaces <- function(detailed = F) {

  LTE <- .lte_env.()


  lst <- LTE$workspaces
  if(nrow(lst) > 0 & ! detailed) lst <- lst$name

  lst
}
# =============================================================================.
# Create workspace folders required by LittleThumb
# -----------------------------------------------------------------------------.
create_workspace <- function(name, path = NULL) {

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
delete_workspace <- function(name) {

  LTE <- .lte_env.()

}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
open_workspace <- function(name, path = NULL) {

  LTE <- .lte_env.()
  lbl <- name

  chk <- td_selector(LTE$workspaces, name == lbl, v = "is_created")
  if(! chk) stop("workspace has to be created before it can be opened")

  chk <- td_selector(LTE$workspaces, name == lbl, v = "is_opened")
  if(! chk) {
    assign(lbl, new.env(), pos = globalenv())
    register_value(LTE$workspaces, x = lbl, v = "is_opened") <- T
  }

  # Auto load existing datasets and jobs

}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
close_workspace <- function(name) {

  LTE <- .lte_env.()
  lbl <- name

  chk <- td_selector(LTE$workspaces, name == lbl, v = "is_opened")
  if(chk) {
    rm(list = lbl, pos = globalenv())
    register_value(LTE$workspaces, x = lbl, v = "is_opened") <- F
  }
}
