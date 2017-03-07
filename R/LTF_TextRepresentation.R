# GENERIC ######################################################################

# =============================================================================.
# Interfaces
# -----------------------------------------------------------------------------.
obj2txt <- function (obj, ...) { UseMethod("obj2txt", obj) }
txt2obj <- function (txt, ...) { UseMethod("txt2obj", txt) }
# -----------------------------------------------------------------------------.
obj2txt.default <- function (obj, ...) { NextMethod("obj2txt", obj, ...) }
txt2obj.default <- function (obj, ...) { NextMethod("txt2obj", obj, ...) }
# -----------------------------------------------------------------------------.

# FUNCTIONS ####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
valid_id <- function(x, type = "R", error = F) {

  rxp <- ""
  if(type == "R")  rxp <- "^[\\.a-z][\\._a-z0-9]+$"
  if(type == "LT") rxp <- "^[\\._a-z0-9]+$"
  if(rxp == "") stop("invalid type")

  chk <- grepl(rxp, x, perl = T, ignore.case = T)
  if(error & ! chk) stop("invalid string")
  chk
}
# =============================================================================.
#' make_id
# -----------------------------------------------------------------------------.
#' @param n
#' @param tag
# -----------------------------------------------------------------------------.
#' @return \code{character}
# -----------------------------------------------------------------------------.
make_id <- function(n = 1, tag = "") {
  tag <- paste0(tag, format(Sys.time(), "%Y%m%d_%H%M_"))
  key <- sapply(rep("", n), tempfile, tmpdir = "")
  paste0(tag, basename(key))
}
# =============================================================================.
# Silent lines
# -----------------------------------------------------------------------------.
remove_silent_lines <- function(txt) {
  rxp <- "^#( |#|=|-|/)+$"
  idx <- which(grepl(rxp, txt, perl = T))
  if(length(idx) > 0) txt <- txt[-idx]
  txt
}
# =============================================================================.
# Block signature : make | LT_MetaData
# -----------------------------------------------------------------------------.
make_md_signature <- function(obj, name = NULL) {
  txt <- ""
  if(is.null(name)) name <- attr(obj, "lt_name")
  if(is.null(name)) name <- ""
  if(name != "") {
    chk <- valid_id(name, error = T)
    txt <- paste0(" : ", name)
  }
  txt <- paste0(" jsonlite", txt, " ////")
  txt <- paste0("# ////", str_pad(txt, pad = "/", width = 74))
  txt
}
# =============================================================================.
# Block signature : make | LT_TabularData
# -----------------------------------------------------------------------------.
make_td_signature <- function(obj, name = NULL, rn = F) {
  txt <- ""
  if(is.null(name)) name <- attr(obj, "lt_name")
  if(is.null(name)) name <- ""
  if(name != "") {
    chk <- valid_id(name, error = T)
    txt <- paste0(" : ", name)
  }
  if(rn) txt <- paste0(txt, " (with row names in the unnamed 1st column)")
  txt <- paste0("# Tabular data", txt)
  txt
}
# =============================================================================.
# Block signature : read | LT_MetaData
# -----------------------------------------------------------------------------.
read_md_signature <- function(txt) {
  # Extract jsonlite data tag
  rxp <- "^# /+ jsonlite( +\\: +([\\._a-z0-9]+))? /+$"
  idx <- which(grepl(rxp, txt, perl = T, ignore.case = T))
  if(length(idx) != 1) stop("invalid format")

  ltn <- gsub(rxp, "\\2", txt[idx], perl = T, ignore.case = T)
  ltn <- str_trim(ltn)

  list(name = ltn, index = idx)
}
# =============================================================================.
# Block signature : read | LT_TabularData
# -----------------------------------------------------------------------------.
read_td_signature <- function(txt) {
  # Extract tabular data tag
  rxp <- "^# Tabular data( +\\: +([\\._a-z0-9]+)($| +))?.*$"
  idx <- which(grepl(rxp, txt, perl = T, ignore.case = T))
  if(length(idx) != 1) stop("invalid format")

  ltn <- gsub(rxp, "\\2", txt[idx], perl = T, ignore.case = T)
  ltn <- str_trim(ltn)

  list(name = ltn, index = idx)
}
# =============================================================================.
# Object signature : make
# -----------------------------------------------------------------------------.
make_obj_signature <- function(obj, name = NULL) {

  if(is.null(name)) name <- obj$name
  if(is.null(name)) name <- ""
  if(name != "") {
    chk <- valid_id(name, error = T)
    txt <- paste0(" | ", name)
  }
  txt <- paste0(" : ", obj$id, txt)
  txt <- paste0("# Object = ", as.character(class(obj)[1]), txt)

  txt
}
# =============================================================================.
# Object signature : read
# -----------------------------------------------------------------------------.
read_obj_signature <- function(txt) {

  # Extract and parse signature
  rxp <- "[^ \\:\\=\\|]+"
  rxp <- paste0("(", rxp, ") *: *(", rxp, ") *(\\| (", rxp, ") *)?")
  rxp <- paste0("^# +Object += *", rxp, "$")

  idx <- which(grepl(rxp, txt, perl = T))
  if(length(idx) != 1) stop("missing object signature")

  cls <- gsub(rxp, "\\1", txt[idx], perl = T)
  id  <- gsub(rxp, "\\2", txt[idx], perl = T)
  ltn <- gsub(rxp, "\\4", txt[idx], perl = T)
  txt <- txt[-idx]

  list(class = cls, id = id, name = ltn, index = idx)
}
