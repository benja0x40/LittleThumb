# COMMON #######################################################################

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Provide default values to unspecified arguments.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DefaultArgs <- function(
  default, ignore = NULL, from = NULL, to = NULL, origin = parent.frame()
) {

  lbl <- deparse(substitute(to))
  lst <- names(default)

  if(is.null(from)) from <- origin
  if(is.null(to)) to <- origin

  if(is.function(from)) {
    lst <- methods::formalArgs(from)
    from <- parent.frame()
  }

  lst <- setdiff(lst, ignore)
  if(is.environment(to)) {
    for(a in lst) {
      if(is.null(to[[a]]) & ! (is.null(from[[a]]) | identical(from, to))) {
        to[[a]] <- from[[a]]
      }
      if(is.null(to[[a]]) & ! is.null(default[[a]])) {
        to[[a]] <- default[[a]]
      }
    }
  } else {
    for(a in lst) {
      if(is.null(origin[[to]][[a]]) & ! (is.null(from[[a]]))) {
        origin[[to]][[a]] <- from[[a]]
      }
      if(is.null(origin[[to]][[a]]) & ! is.null(default[[a]])) {
        origin[[to]][[a]] <- default[[a]]
      }
    }
  }
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Standardize the length of vector arguments.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
VectorArgs <- function(lst, from = NULL, size = NULL) {

  if(is.null(from)) from <- parent.frame()
  if(is.null(size)) {
    size <- 0
    for(x in lst) size <- max(size, length(from[[x]]))
  }

  for(x in lst) from[[x]] <- rep(from[[x]], length.out = size)

  if(! is.environment(from)) from
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Standardize the value of clonal arguments.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ClonalArg <- function(u, a, d) { # user value, arg names, default value

  n <- length(a)
  r <- rep(list(d), n)
  names(r) <- a

  if(is.null(names(u))) {
    d[] <- rep(u, length.out = length(d))
    r[] <- rep(list(d), n)
  } else {
    u <- lapply(u, rep, length.out = length(d))
    for(k in names(u)) r[[k]][] <- u[[k]]
  }

  r
}

# LittleThumb ##################################################################

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Standardize the value of a logical argument.
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
    r[] <- TRUE
  } else {
    names(r) <- x
    r[] <- as.logical(a)
  }

  r <- ifelse(is.null(r[[x]]), FALSE, r[[x]])

  r
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Resolve the value of a character argument.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
NamedArg <- function(x, a) {
  if(is.null(names(a))) {
    x <- a
  } else {
    if(is.list(a)) x <- a[[x]] else x <- a[x]
  }
  x
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' List of arguments involving a delayed expression evaluation.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ManageObjectAndParentArgs <- function(a, origin = parent.frame()) {

  if(length(a) < 2) stop("insufficient arguments")
  if(is.null(names(a)) | identical(names(a)[2], "")) names(a)[2] <- "obj"

  obj.name <- a[["name"]]
  if(is.null(obj.name) & "obj" %in% names(a)) {
    obj.name <- as.character(a["obj"])
    a["name"] <- obj.name
  }
  if(is.null(obj.name)) stop("missing 'obj' argument")

  prn.name <- a[["parent.name"]]
  if(is.null(prn.name) & ! is.null(a[["parent"]])) {
    prn.name <- as.character(a["parent"])
    a["parent.name"] <- prn.name
  }

  if(! IsKnowObject(obj.name)) RegisterObject(obj.name)
  if(! is.null(prn.name)) {
    if(! IsKnowObject(prn.name)) stop("unknown parent object ", prn.name)
    else SetParent(obj.name, prn.name)
  }

  a[1] <- call("list")

  a
}
