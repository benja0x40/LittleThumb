# INCLUDES #####################################################################

# Generic and regular functions
#' @include LTF_Basics.R
#' @include LTF_R_Objects.R
#' @include LTF_Registration.R
#' @include LTF_TextRepresentation.R
#' @include LTF_FileSystem.R


# Class definitions & implementations





# S3 DEFINTION #################################################################

# =============================================================================.
# Inherits from: list
# =============================================================================.
#' LT_MetaData
# -----------------------------------------------------------------------------.
#' @export
#' @description
#'
#' @param ...
#'
#' @return
#' a list (using class LT_MetaData would to fix dispatch issues)
# -----------------------------------------------------------------------------.
LT_MetaData <- function(..., .names. = NULL) {
  obj <- list(...)
  if(! is.null(.names.)) names(obj) <- .names.
  # obj <- add_class(obj, "LT_MetaData")
  obj
}
# -----------------------------------------------------------------------------.
# is.LT_MetaData <- function(x) { inherits(x, "LT_MetaData") }
# -----------------------------------------------------------------------------.
# print.LT_MetaData <- function(x, ...) { NextMethod(x, ...) }
# -----------------------------------------------------------------------------.

# METHODS ######################################################################

# > TextRepresentation #########################################################

# =============================================================================.
# Represent data as tabular text
# -----------------------------------------------------------------------------.
obj2txt.list <- function(obj, name = NULL, pretty = T, indent = 2) {

  txt <- ""
  msg <- make_md_signature(obj, name = name)

  if(length(obj) > 0) {
    # Write data as jsonlite
    txt <- toJSON(obj)
    if(pretty) txt <- prettify(txt, indent = indent)
    txt <- unlist(str_split(txt, "\n"))

    # Insert block signature
    txt <- c(msg, paste0("# ", txt))
  }

  txt <- add_class(txt, "LT_MetaData")
  txt
}
# =============================================================================.
# txt should be a character vector resulting from readLines
# -----------------------------------------------------------------------------.
txt2obj.LT_MetaData <- function(txt, ...) {

  obj <- LT_MetaData()

  # Remove silent lines and extract block signature
  txt <- remove_silent_lines(txt)
  sgn <- read_md_signature(txt)

  # Parse data
  txt <- gsub("^#+", "", txt[- sgn$index], perl = T)
  txt <- paste(txt, collapse = "\n")
  if(txt != "") {
    obj <- as(fromJSON(txt, ...), Class = "list")
    if(sgn$name != "") attr(obj, "lt_name") <- sgn$name
  }

  obj
}

# > Registration ###############################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
is_key.list <- function(obj, x, error = F) {

  if(is.character(x)) chk <- x %in% names(obj)
  if(is.numeric(x)) chk <- x  > 0 & x <= length(obj)

  if(error) checklist(chk, x, "undefined element(s)", spc = " ")
  all(chk)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
is_registered.list <- function(obj, k = 1, x, lbl = "", error = F) {

  if(error & length(obj) == 0) stop("empty register")
  chk <- is_key(obj, k, error = T)

  msg <- paste("undefined", lbl)
  chk <- is_key.list(obj[[k]], x)
  if(error) checklist(chk, x, msg, spc = " ")

  all(chk)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
# register_value.list <- function(obj, k = 1, x, lbl = "") {
#
#   chk <- is_registered(obj, k = k, x, lbl = lbl, error = T)
#
#   nk <- length(k)
#   nx <- length(x)
#
#   if(nk >  1 & nx >  1) stop("multiple indices not supported")
#   if(nk == 1 & nx == 1) obj <- obj[[k]][[x]]
#   if(nk == 1 & nx >  1) obj <- obj[[k]][x]
#   if(nk >  1 & nx == 1) obj <- lapply(obj[k], "[[", x)
#
#   obj
# }
# =============================================================================.
#
# -----------------------------------------------------------------------------.
# `register_value<-.list` <- function(obj, k = 1, x, lbl = "", value) {
#
#   chk <- is_registered(obj, k = k, x, lbl = lbl, error = T)
#
#   nk <- length(k)
#   nx <- length(x)
#
#   if(nk >  1 & nx >  1) stop("multiple indices not supported")
#   if(nk == 1 & nx == 1) obj[[k]][[x]] <- value
#   if(nk == 1 & nx >  1) obj[[k]][x] <- value
#   if(nk >  1 & nx == 1) stop("multiple indices not supported")
#
#   obj
# }
# =============================================================================.
#
# -----------------------------------------------------------------------------.
# register_filter.list <- function(obj, x, rn = F) {
#
#   lst <- all.vars(x)
#   chk <- is_key(obj, lst, error = T)
#
#   # cls <- class(obj)
#   r <- as_data_frame(obj)
#
#   rnc <- make_id(tag = "RN")
#   r <- protect_row_names(r, rnc)
#
#   if(is.character(x)) r <- dplyr::filter_(r, x)
#   else if(is.call(x)) r <- dplyr::filter_(r, .dots = lazyeval::as.lazy(x))
#   else stop("x must be a call or character")
#
#   obj <- obj[r[[rnc]], ]
#   if(! rn) rownames(obj) <- NULL
#
#   obj
# }
# =============================================================================.
#
# -----------------------------------------------------------------------------.
# register_split.list <- function(obj, x, ids = T, rn = F) {
#
#   lst <- all.vars(x)
#   chk <- is_key(obj, lst, error = T)
#
#   if(is.character(x) & length(x) == 1) obj <- split(obj, f = obj[[x]])
#   else if(length(x) == length(obj)) obj <- split(obj, f = x)
#   else stop("x must be a factor or character")
#
#   if(! ids) names(obj) <- NULL
#
#   obj
# }
# =============================================================================.
#
# -----------------------------------------------------------------------------.
register_merge.list <- function(
  ..., ids = NULL, ids_last = T, full = T, rn = F
) {

  obj <- list(...)
  if(length(obj) == 1 & is.list(obj[[1]][[1]])) {
    obj <- c(obj[[1]], ids = ids, ids_last = ids_last, full = full, rn = rn)
    obj <- do.call(register_merge, args = obj)
  } else {
    # obj <- do.call(c, args = obj)
  }
  obj
}

