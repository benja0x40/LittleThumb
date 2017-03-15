# INCLUDES #####################################################################

# Generic and regular functions
#' @include LTF_Basics.R
#' @include LTF_R_Objects.R
#' @include LTF_Registration.R
#' @include LTF_TextRepresentation.R
#' @include LTF_FileSystem.R


# Class definitions & implementations
#' @include LTC_TabularData.R
#' @include LTC_MetaData.R



# S3 DEFINTION #################################################################

# =============================================================================.
# Composed of: LT_MetaData, LT_TabularData
# =============================================================================.
#' LT_Data
# -----------------------------------------------------------------------------.
#' @description
#'
#' @param ...
#'
#' @return
#' an object of class LT_Data
# -----------------------------------------------------------------------------.
LT_Data <- function(name = NULL, description = NULL, MD = NULL, TD = NULL) {

  if(is.null(name)) name <- ""
  if(is.null(description)) description <- ""
  if(is.null(MD)) MD <- LT_MetaData()
  if(is.null(TD)) TD <- LT_TabularData()

  obj <- list(id = make_id(), name = name, MD = MD, TD = TD)

  obj <- add_class(obj, "LT_Data")
  obj
}
# -----------------------------------------------------------------------------.
is.LT_Data <- function(x) { inherits(x, "LT_Data") }
# -----------------------------------------------------------------------------.

# METHODS ######################################################################

# > TextRepresentation #########################################################

# =============================================================================.
# Represent data as tabular text
# -----------------------------------------------------------------------------.
obj2txt.LT_Data <- function(obj, name = NULL, rn = F, pretty = T, indent = 2) {

  txt <- ""
  msg <- ""

  chk <- valid_id(obj$id, type = "LT", error = T)

  if(is.null(name)) name <- obj$name
  if(is.null(name)) name <- ""
  if(name != "") {
    chk <- valid_id(name, error = T)
    msg <- paste0(" | ", name)
  }
  msg <- paste0(" : ", obj$id, msg)
  msg <- paste0("# Object = ", as.character(class(obj)[1]), msg)

  MD_txt <- obj2txt(obj$MD, name = "MD", pretty = pretty, indent =  indent)
  TD_txt <- obj2txt(obj$TD, name = "TD", rn = rn)

  txt <- c(
    str_pad("# ", pad = "=", width = 80, side = "right"),
    # Object = class : id | name
    msg,
    MD_txt,
    str_pad("# ", pad = "/", width = 80, side = "right"),
    TD_txt
  )
  # Cleanup
  txt <- txt[! grepl("^# *$", txt, perl = T)]

  txt <- add_class(txt, "LT_Data")
  txt
}
# =============================================================================.
# txt should be a character vector resulting from readLines
# -----------------------------------------------------------------------------.
txt2obj.LT_Data <- function(txt, ...) {

  blk2obj <- function(obj, txt, cls) {
    txt <- add_class(txt, cls)
    blk <- txt2obj(txt)
    ltn <- attributes(blk)[["lt_name"]]
    attributes(blk)[["lt_name"]] <- NULL
    obj[[ltn]] <- blk
    obj
  }

  # Remove silent lines and extract object signature
  txt <- remove_silent_lines(txt)
  sgn <- read_obj_signature(txt)
  txt <- txt[- sgn$index]

  # Make object
  obj <- do.call(what = sgn$class, args = list())
  obj$id <- sgn$id
  if(sgn$name != "") obj$name <- sgn$name

  # Locate data blocks
  mdx <- read_md_signature(txt)$index
  tdx <- read_td_signature(txt)$index
  end <- length(txt)

  # Parse data blocks
  if(length(mdx) == 1) {
    blk <- mdx : ifelse(mdx > tdx, end, tdx - 1)
    obj <- blk2obj(obj, txt[blk], cls = "LT_MetaData")
  }
  if(length(tdx) == 1) {
    blk <- tdx : ifelse(tdx > mdx, end, mdx - 1)
    obj <- blk2obj(obj, txt[blk], cls = "LT_TabularData")
  }

  obj
}
