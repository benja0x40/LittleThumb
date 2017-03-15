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
# Inherits from: data.frame
# =============================================================================.
#' LT_TabularData
# -----------------------------------------------------------------------------.
#' @description
#'
#' @param ...
#'
#' @return
#' a data.frame (using class LT_TabularData would to fix dispatch issues)
# -----------------------------------------------------------------------------.
LT_TabularData <- function(...) {
  obj <- data.frame(..., stringsAsFactors = F)
  # obj <- add_class(obj, "LT_TabularData")
  obj
}
# -----------------------------------------------------------------------------.
# is.LT_TabularData <- function(x) { inherits(x, "LT_TabularData") }
# -----------------------------------------------------------------------------.
# `$.LT_TabularData`  <- function(x, ...) { NextMethod(x, ...) }
# `$<-.LT_TabularData`  <- function(x, ...) { NextMethod(x, ...) }
# `[[.LT_TabularData`  <- function(x, ...) { NextMethod(x, ...) }
# `[[<-.LT_TabularData`  <- function(x, ...) { NextMethod(x, ...) }
# `[.LT_TabularData`  <- function(x, ...) { NextMethod(x, ...) }
# `[<-.LT_TabularData`  <- function(x, ...) { NextMethod(x, ...) }
# -----------------------------------------------------------------------------.
# print.LT_TabularData <- function(x, ...) { NextMethod(x, ...) }
# -----------------------------------------------------------------------------.

# METHODS ######################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
td_selector <- function(obj, x = NULL, v = NULL) {

  if(! is.data.frame(obj)) stop("obj must be a data.frame")

  x <- lazy(x)$expr
  if(is.character(x)) x <- parse(text = eval(x))

  if(! is.null(x)) {
    lst <- all.vars(x)
    lst <- lst[! sapply(lst, exists, where = parent.frame())]
    chk <- is_key(obj, lst, error = T)

    x <- eval(x, envir = obj, enclos = parent.frame())
  } else {
    x <- rep(T, nrow(obj))
  }

  if(! is.null(v)) {
    chk <- is_key(obj, v, error = T)
    x <- obj[x, v]
  }

  x
}

# > TextRepresentation #########################################################

# =============================================================================.
# Represent data as tabular text
# -----------------------------------------------------------------------------.
obj2txt.data.frame <- function(obj, name = NULL, rn = F) {

  txt <- ""
  msg <- make_td_signature(obj, name = name, rn = rn)

  if(nrow(obj) > 0) {
    # Write data as tab separated values
    txt <- textConnection("tmp", open = "w", local = T)
    write.table(
      obj, txt, quote = F, sep = "\t", row.names = rn, col.names = T
    )
    # Insert block signature
    txt <- c(msg, str_pad("# ", pad = "=", width = 80, side = "right"), tmp)
  }

  txt <- add_class(txt, "LT_TabularData")
  txt
}
# =============================================================================.
# txt should be a character vector resulting from readLines
# -----------------------------------------------------------------------------.
txt2obj.LT_TabularData <- function(txt) {

  obj <- LT_TabularData()

  # Remove silent lines and extract block signature
  txt <- remove_silent_lines(txt)
  sgn <- read_td_signature(txt)

  # Parse data
  txt <- paste(txt[- sgn$index], collapse = "\n")
  if(txt != "") {
    obj <- read.delim(
      textConnection(txt), comment.char = "#", header = T, stringsAsFactors = F
    )
    obj <- LT_TabularData(obj)
    if(sgn$name != "") attr(obj, "lt_name") <- sgn$name
  }

  obj
}

# > Registration ###############################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
is_key.data.frame <- function(obj, x, error = F) {

  chk <- T

  if(nrow(obj) == 0) chk <- F
  if(error & ! chk) stop("empty register")

  if(all(chk) & is.character(x)) chk <- x %in% colnames(obj)
  if(all(chk) & is.numeric(x)) chk <- x  > 0 & x <= ncol(obj)

  if(error) checklist(chk, x, "undefined column(s)", spc = " ")
  all(chk)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
is_registered.data.frame <- function(obj, k = 1, x, lbl = "", error = F) {

  chk <- is_key(obj, k, error = error)

  if(chk) {
    msg <- paste("undefined", lbl)
    chk <- x %in% obj[[k]]
    if(error) checklist(chk, x, msg, spc = " ")
  }

  all(chk)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
register_value.data.frame <- function(obj, k = 1, x, v = 2, lbl = "") {

  nk <- length(k)
  if(nk >  1) stop("multiple indices not supported")

  chk <- is_registered(obj, k = k, x, lbl = lbl, error = T)
  chk <- is_key(obj, v, error = T)

  x <- match(x, obj[[k]])
  obj[x, v]
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
`register_value<-.data.frame` <- function(
  obj, k = 1, x, v = 2, lbl = "", value
) {

  nk <- length(k)
  if(nk >  1) stop("multiple indices not supported")

  chk <- is_registered(obj, k = k, x, lbl = lbl, error = T)
  chk <- is_key(obj, v, error = T)

  x <- match(x, obj[[k]])
  obj[x, v] <- value

  obj
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
register_filter.data.frame <- function(obj, x, rn = F) {

  x <- td_selector(obj, x)
  obj <- obj[x, ]
  if(! rn) rownames(obj) <- NULL
  obj
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
register_split.data.frame <- function(obj, x, ids = T, rn = F) {

  lst <- all.vars(x)
  chk <- is_key(obj, lst, error = T)

  if(is.character(x) & length(x) == 1) obj <- split(obj, f = obj[[x]])
  else if(length(x) == nrow(obj)) obj <- split(obj, f = x)
  else stop("x must be a factor or character")

  if(! ids) names(obj) <- NULL

  obj
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
register_merge.data.frame <- function(
  ..., ids = NULL, ids_last = T, full = T, rn = F
) {

  rnc <- make_id(tag = "RN")
  obj <- list(...)

  # Shared columns
  lst <- lapply(obj, colnames)
  lst <- Reduce(base::intersect, lst)
  lst <- c(ids, lst)

  # Bind rows
  obj <- lapply(obj, rownames2col, rnc = rnc)
  obj <- lapply(obj, dplyr::as_data_frame)
  obj <- do.call("bind_rows", args = list(obj, .id = ids))
  obj <- base::as.data.frame(obj, stringsAsFactors = F)
  obj <- LT_TabularData(obj)

  # Cleanup
  if(rn) obj <- col2rownames(obj, rnc = rnc)
  if(ids_last & ! is.null(ids)) obj <- obj[, c(2:ncol(obj), 1)]
  if(! full) obj <- obj[colnames(obj) %in% lst]

  obj
}
