# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @description
#' Provide default values to unspecified arguments
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
DefaultArgs <- function(default, ignore = NULL, from = NULL, to = NULL) {

  if(is.null(to)) to <- parent.frame()

  lst <- names(default)
  if(is.function(from)) lst <- methods::formalArgs(from)

  lst <- setdiff(lst, ignore)

  for(a in lst) {
    if(is.null(to[[a]]) & ! is.null(default[[a]])) {
      to[[a]] <- default[[a]]
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
#' Resolve the value of a character argument
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
#' List of arguments involving a delayed expression evaluation
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ObjWithExpressionArgs <- function(a, xpr, explicit = "name") {

  env <- parent.frame()
  xpr <- deparse(substitute(xpr))

  n   <- length(a)
  lst <- names(a)
  if(is.null(lst)) lst <- rep("", n)

  e <- match(explicit, lst)
  i <- 1 + match("", lst[-1])
  x <- n - match("", rev(lst[-1])) + 1

  if(is.na(x)) stop("missing expression")
  if(x <= i) i <- NA

  env[[xpr]] <- a[[x]]
  a[[x]] <- NULL

  if(! is.na(i)) {
    implicit <- as.character(a[[i]])
    a[[i]] <- NULL
  }

  a[1] <- call("list")
  a <- as.environment(eval(a))

  if(is.na(e)) a[[explicit]] <- implicit

  a
}
