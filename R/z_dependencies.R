# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
LT <- new.env()
LT$parent <- list()

# LT$context <- NA
# LT$history <- list()
# LT$options <- list()
# LT$default <- DefaultOptions()
# LT$objects <- data.frame()

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ChangeContext <- function(x, inherit = NA) {
  if(is.null(LT$options[[x]])) {
    if(! is.null(LT$options[[inherit]])) {
      LT$options[[x]] <- LT$options[[inherit]]
    } else {
      LT$options[[x]] <- DefaultOptions()
    }
  }
  if(! is.na(LT$context)) LT$history <- c(LT$history, LT$context)
  LT$context <- x
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RestoreContext <- function(x) {
  n <- length(LT$history)
  if(n > 0) {
    LT$context <- LT$history[[n]]
    LT$history[[n]] <- NULL
  } else {
    LT$context <- NA
  }
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
CurrentContext <- function() {
  LT$context
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
CurrentOptions <- function() {
  if(is.na(LT$context)) opt <- LT$default
  else opt <- LT$options[[LT$context]]
  opt
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
GetParents <- function(child) {
  r <- LT$parent[[child]]
  if(identical(r, NA)) r <- NULL
  if(! is.null(r)) r <- c(GetParents(r), r)
  r
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
GetChilds <- function(parent) {
  names(which(LT$parent == parent))
}

# SetParent("a", "test")
# SetParent("x", "a")
# SetParent("y", "a")
# SetParent("z", "a")


# f <- function(x) {
#   print(deparse(substitute(x)))
# }
