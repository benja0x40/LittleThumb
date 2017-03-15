# GENERIC ######################################################################

# =============================================================================.
# Interfaces
# -----------------------------------------------------------------------------.
rownames2col <- function (x, ...) { UseMethod("rownames2col", x) }
col2rownames <- function (x, ...) { UseMethod("col2rownames", x) }
# -----------------------------------------------------------------------------.
rownames2col.default <- function (x, ...) { NextMethod("rownames2col", x, ...) }
col2rownames.default <- function (x, ...) { NextMethod("col2rownames", x, ...) }
# -----------------------------------------------------------------------------.

# data.frame ###################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
rownames2col.data.frame <- function(x, rnc) {
  x[[rnc]] <- as(rownames(x), class(attributes(x)$row.names))
  rownames(x) <- NULL
  x
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
col2rownames.data.frame <- function(x, rnc) {
  rownames(x) <- x[[rnc]]
  x[[rnc]] <- NULL
  x
}

# S3 class #####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
add_class <- function(obj, name) {
  class(obj) <- append(name, class(obj))
  obj
}
# =============================================================================.
# NOT USED & UNTESTED
# -----------------------------------------------------------------------------.
# match_class <- function(..., class, strict = T, error = F) {
#   lst <- list(...)
#   if(length(lst) != length(class)) {
#     stop("number of classes and arguments are inconsistent")
#   }
#   class(obj) <- append(name, class(obj))
#   obj
# }
# =============================================================================.
#' check_class
# -----------------------------------------------------------------------------.
#' @description
#' verify if R object belong to requested classes
#'
#' @param x a list of R objects or an R object
#' @param class \code{character} name
#' @param lst \code{logical} if TRUE then x is tested otherwise elements of x
#' @param strict \code{logical} if TRUE then x must be member of all classes
# -----------------------------------------------------------------------------.
#' @return check_class returns a \code{logical} value
# -----------------------------------------------------------------------------.
check_class <- function(x, class, lst = F, strict = T, error = F) {
  lbl <- deparse(substitute(x))
  class <- unique(class)
  if(lst | length(x) == 0) x <- list(x)
  x <- sapply(x, is, simplify = F)
  x <- sapply(x, base::intersect, y = class, simplify = F)
  x <- sapply(x, length)
  x <- ifelse(strict, all(x == length(class)), all(x > 0))
  if(error & ! x) stop(lbl, " must be a ", class)
  x
}
# =============================================================================.
#' Reassign object to a different environment
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{assign},
#'   \link{environment}
# -----------------------------------------------------------------------------.
#' @description
#' copy or move an R object from the source environment \code{src} to the target
#' environment \code{pos}.
#'
#' @param obj
#' an R object or the name of an R object
#'
#' @param pos
#' specifies a target \link{environment} where the object should be reassigned.
#' See \link{assign} for documentation on the different ways to specify
#' environments with the \code{pos} paramter.
#'
#' @param src
#' specifies the source \link{environment} where the object is currently located
#' (default = .GlobalEnv).
#' See \link{assign} for documentation on the different ways to specify
#' environments with the \code{pos} paramter.
#'
#' @param keep
#' logical indicating if the object should be removed or preserved
#' at its current location (defautl = F, remove).
#'
#' @return NULL
# -----------------------------------------------------------------------------.
reassign <- function(obj, pos, src = globalenv(), keep = F) {

  # R object identifier
  obj.name <- deparse(substitute(obj))

  lst <- ls(src)
  if(length(obj) == 1 & is.character(obj)) {
    if(obj %in% lst & ! obj.name %in% lst) {
      obj.name <- obj
      obj <- get(obj.name, pos = src)
    }
  }

  assign(obj.name, obj, pos = pos)
  if(! keep) {
    rm(list = obj.name, pos = src)
  }
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
# make_class_attr <- function(name, constructor) {
#   cs3 <- is(do.call(constructor, args = list()))
#   cs4 <- base::intersect(cs3, .OldClassesList)
#   cs3 <- append(name, cs3)
#   cs4 <- append(name, cs4)
#   list(S3 = cs3, S4 = cs4)
# }
# =============================================================================.
#
# -----------------------------------------------------------------------------.
# make_object <- function(name, constructor, ...) {
#   cls <- make_class_attr(name, constructor)
#   obj <- do.call(constructor, args = list(...))
#   class(obj) <- cls$S3
#   # attr(obj, "constructor") <- constructor
#   obj
# }
# =============================================================================.
# NOT USED & UNTESTED
# -----------------------------------------------------------------------------.
# derive_class <- function(constructor, name) {
#
#   cls <- make_class_attr(constructor, name)
#
#   f <- name
#   assign(
#     f, function(...) {
#       x <- do.call(constructor, args = list(...))
#       class(x) <- cls$S3
#       x
#     },
#     pos = parent.env(environment())
#   )
#
#   f <- paste0("is.", name)
#   assign(f, function(x) { inherits(x, name) }, pos = parent.env(environment()))
# }

