# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
LT <- new.env()
LT$parent <- list()

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ResetRegistry <- function() {
  LT$parent <- list()
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
IsKnowObject <- function(x) {
  ! is.null(LT$parent[[x]])
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RegisterObject <- function(x) {
  LT$parent[[x]] <- NA
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ForgetObject <- function(x) {
  LT$parent[[x]] <- NULL
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SetParent <- function(child, parent) {
  LT$parent[[child]] <- parent
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
GetParents <- function(x) {
  r <- LT$parent[[x]]
  if(identical(r, NA)) r <- NULL
  if(! is.null(r)) r <- c(GetParents(r), r)
  r
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
GetChilds <- function(x) {
  names(which(LT$parent == x))
}
