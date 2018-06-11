# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
ObjectExists <- function(
  x, prn.name = NULL, parent = NULL, origin = parent.frame(), inherits = FALSE
) {
  r <- FALSE
  if(! is.null(prn.name)) {
    if(! is.null(origin[[prn.name]])) {
      r <- exists(x, where = origin[[prn.name]], inherits = inherits)
    }
  } else {
    if(! is.null(parent)) r <- exists(x, where = parent, inherits = inherits)
    else                  r <- exists(x, where = origin, inherits = inherits)
  }
  r
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
GetValue <- function(
  x, prn.name = NULL, parent = NULL, origin = parent.frame()
) {
  value <- NULL
  if(! is.null(prn.name)) {
    value <- origin[[prn.name]][[x]]
  } else {
    if(! is.null(parent)) value <- parent[[x]]
    else                  value <- origin[[x]]
  }
  value
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SetValue <- function(
  x, prn.name = NULL, parent = NULL, origin = parent.frame(), value
) {
  if(! is.null(prn.name)) {
    origin[[prn.name]][[x]] <- value
  } else {
    if(! is.null(parent)) parent[[x]] <- value
    else                  origin[[x]] <- value
  }
}

# =============================================================================.
#' ** RESERVED FOR INTERNAL USE **
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
RemoveObject <- function(
  x, prn.name = NULL, parent = NULL, origin = parent.frame()
) {
  SetValue(x, prn.name, parent, origin, value = NULL)
  if(ObjectExists(x, prn.name, parent, origin)) {
    if(! is.null(prn.name)) {
      rm(list = x, pos = origin[[prn.name]])
    } else {
      if(! is.null(parent)) rm(list = x, pos = parent)
      else                  rm(list = x, pos = origin)
    }
  }
}
