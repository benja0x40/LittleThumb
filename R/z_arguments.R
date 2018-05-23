# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Provide default values to unspecified arguments
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DefaultArgs <- function(cfg, ignore = NULL, fun = NULL, env = NULL) {

  if(is.null(env)) env <- parent.frame()

  lst <- names(cfg)
  if(! is.null(fun)) lst <- methods::formalArgs(fun)

  lst <- setdiff(lst, ignore)

  for(a in lst) {
    if(is.null(env[[a]]) & ! is.null(cfg[[a]])) {
      env[[a]] <- cfg[[a]]
    }
  }
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Standardize the value of a logical argument
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
LogicalArg <- function(x, a) {

  r <- vector("list", length(a))

  if(! is.null(names(a))) {
    names(r) <- names(a)
    r[] <- as.logical(a)
  } else if(is.character(a)) {
    names(r) <- a
    r[] <- T
  } else {
    names(r) <- x
    r[] <- as.logical(a)
  }

  r <- ifelse(is.null(r[[x]]), F, r[[x]])

  r
}
