# =============================================================================.
#' checklist
# -----------------------------------------------------------------------------.
#' @param chk
#' @param lst
#' @param msg
# -----------------------------------------------------------------------------.
#' @return NULL
# -----------------------------------------------------------------------------.
checklist <- function(chk, lst, msg = "") {
  if(! all(chk)) {
    msg <- paste0(msg, "\n", paste(lst[! chk], collapse = "\n", sep = ""))
    stop(msg)
  }
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

# LT_Description ###############################################################

# =============================================================================.
#' LT_Description
# -----------------------------------------------------------------------------.
#' @description This S4 class implements an hybrid data structure combining
#' a list and a data frame.
# -----------------------------------------------------------------------------.
setClass(
  "LT_Description",
  representation = representation(
    id = "character", properties = "list", data = "DataFrame"
  ),
  prototype = prototype(
    id = make_id(), properties = list(name = ""), data = DataFrame()
  )
)
# -----------------------------------------------------------------------------.
# Top level names of the properties list
setGeneric(
  "properties", function(obj) { standardGeneric("properties") }
)
# Accessor for top level properties (get)
setGeneric(
  "property", function(obj, key) { standardGeneric("property") }
)
# Accessor for top level properties (set)
setGeneric(
  "property<-", function(obj, key, value) { standardGeneric("property<-") }
)
# Column names of the description data frame
setGeneric(
  "datakeys", function(obj) { standardGeneric("datakeys") }
)
# Number of rows in the description data frame
setGeneric(
  "ndata", function(obj) { standardGeneric("ndata") }
)
# =============================================================================.
#' properties
# -----------------------------------------------------------------------------.
#' @describeIn LT_Description top level names of the properties list
#' @description
#' @param obj object
#' @return \code{properties} returns a \code{character} vector
# -----------------------------------------------------------------------------.
setMethod(
  "properties", signature(obj = "LT_Description"),
  definition = function(obj) { names(obj@properties) }
)
# =============================================================================.
#' property
# -----------------------------------------------------------------------------.
#' @describeIn LT_Description accessor for top level properties
#' @description
#' @inheritParams properties
#' @param key property name
# -----------------------------------------------------------------------------.
setMethod(
  "property", signature(obj = "LT_Description", key = "character"),
  definition = function(obj, key) {
    if(length(key) > 1) r <- obj@properties[key] else r <- obj@properties[[key]]
    r
  }
)
# -----------------------------------------------------------------------------.
setMethod(
  "property<-", signature(obj = "LT_Description", key = "character"),
  definition = function(obj, key, value) {
    obj@properties[[key]] <- value
    obj
  }
)
# =============================================================================.
#' datakeys
# -----------------------------------------------------------------------------.
#' @describeIn LT_Description column names of the description data frame
#' @description
#' @inheritParams properties
#' @return \code{datakeys} returns a \code{character} vector
# -----------------------------------------------------------------------------.
setMethod(
  "datakeys", signature(obj = "LT_Description"),
  definition = function(obj) { colnames(obj@data) }
)
# =============================================================================.
#' ndata
# -----------------------------------------------------------------------------.
#' @describeIn LT_Description number of rows in the description data frame
#' @description
#' @inheritParams properties
#' @return \code{ndata} returns an \code{integer}
# -----------------------------------------------------------------------------.
setMethod(
  "ndata", signature(obj = "LT_Description"), function(obj) { nrow(obj@data) }
)

# COMMON PROPERTIES ############################################################

# =============================================================================.
# Also define data accession operators $ , $<-, [, [<- ???
# -----------------------------------------------------------------------------.
lt_id <- function(obj) { obj@id }
# -----------------------------------------------------------------------------.
lt_name <- function(obj) { property(obj, "name") }
`lt_name<-` <- function(obj, value) {
  property(obj, "name") <- value
  obj
}
# -----------------------------------------------------------------------------.
lt_path <- function(obj) { property(obj, "path") }
`lt_path<-` <- function(obj, value) {
  property(obj, "path") <- value
  obj
}

# PARSING & STORAGE ############################################################

# =============================================================================.
#' obj2json
# -----------------------------------------------------------------------------.
#' @param obj object
#' @return \code{character} vector
# -----------------------------------------------------------------------------.
obj2json <- function(obj) {
  txt <- toJSON(obj)
  txt <- prettify(txt, indent = 2)
  txt <- unlist(str_split(txt, "\n"))
  txt
}
# =============================================================================.
#' json2obj
# -----------------------------------------------------------------------------.
#' @param x character vector
#' @return object
# -----------------------------------------------------------------------------.
json2obj <- function(x, Class = "list", ...) {
  as(fromJSON(x, ...), Class)
}
# =============================================================================.
#' obj2txtdef
# -----------------------------------------------------------------------------.
#' @param obj object
#' @return \code{character} vector
# -----------------------------------------------------------------------------.
obj2txtdef <- function(obj, ...) {

  if(! is(obj, "LT_Description")) {
    stop("obj must be a member of class LT_Description")
  }

  # Represent data as tabular text
  txt <- textConnection("tmp", open = "w", local = T)
  rn <- ! is.null(rownames(obj@data))
  write.table(
    obj@data, txt, quote = F, sep = "\t", row.names = rn, col.names = T
  )
  rowmsg <- ifelse(rn, " (with row names in the unnamed 1st column)", "")

  # Represent oject as text definition
  txt <- c(
    # Header: start
    str_pad("# ", pad = "=", width = 80, side = "right"),
    paste0("# Object = ", as.character(class(obj)), " : ", lt_id(obj)),                      # class
    paste0("# ", str_pad(" jsonlite data ////", pad = "/", width = 78)),
    paste0("# ", obj2json(obj@properties)),                              # json
    str_pad("# ", pad = "/", width = 80, side = "right"),
    paste0("# tabular data", rowmsg),
    # Header: end
    str_pad("# ", pad = "=", width = 80, side = "right"),                # table
    tmp
  )

  # Cleanup
  txt <- txt[! grepl("^# *$", txt, perl = T)]
  txt
}
# =============================================================================.
#' txtdef2obj
# -----------------------------------------------------------------------------.
#' @param x character vector
#' @return object
# -----------------------------------------------------------------------------.
txtdef2obj <- function(txt, ...) {

  # Extract class
  rex <- "^# +Object += *([_a-zA-Z]+) *: *([^ ]+) *$"
  chk <- grepl(rex, txt, perl = T)
  cls <- gsub(rex, "\\1", txt[chk], perl = T)
  id  <- gsub(rex, "\\2", txt[chk], perl = T)

  # Extract data
  rex <- "^# *"
  chk <- grepl(rex, txt, perl = T)
  tbl <- paste(txt[! chk], collapse = "\n")
  txt <- gsub(rex, "", txt[chk], perl = T)

  # Extract properties
  rex <- "^(/{4,})"
  idx <- which(grepl(rex, txt, perl = T))
  idx <- (idx[1] + 1):(idx[2] - 1)
  prp <- txt[idx]
  txt <- txt[- idx]

  # Parse data
  tbl <- read.delim(textConnection(tbl), header = T, stringsAsFactors = F)
  tbl <- as(tbl, "DataFrame")

  # Parse properties
  prp <- json2obj(paste(prp, collapse = "\n"), ...)

  obj <- new(Class = cls, properties = prp, data = tbl)
  obj@id <- id

  obj
}
# =============================================================================.
#' check_txtdef
# -----------------------------------------------------------------------------.
#' @param x character vector
#' @return character or NULL
# -----------------------------------------------------------------------------.
check_txtdef <- function(txt, msg = "") {

  msg <- c()

  # Check if header is present
  rex <- "^# *"
  chk <- grepl(rex, txt, perl = T)
  if(sum(chk) == 0) {
    msg <- c(msg, "missing header section")
  } else {
    # Extract tabular and header sections
    tbl <- paste(txt[! chk], collapse = "\n") # tabular
    txt <- gsub(rex, "", txt[chk], perl = T)  # header

    # Check class
    rex <- "^Object += *([_a-zA-Z]+) *: *([^ ]+) *$"
    chk <- grepl(rex, txt, perl = T)
    if(sum(chk) == 0) msg <- c(msg, "missing object statement")

    # Check json block
    rex <- "^(/{4,})"
    idx <- which(grepl(rex, txt, perl = T))
    if(length(idx) != 2) msg <- c(msg, "missing json section")
  }

  if(length(msg) == 0) msg <- NULL

  msg
}
# =============================================================================.
#' save_description
# -----------------------------------------------------------------------------.
#' @param obj object
#' @param x file
# -----------------------------------------------------------------------------.
save_description <- function(obj, x) {
  writeLines(obj2txtdef(obj), x)
}
# =============================================================================.
#' load_description
# -----------------------------------------------------------------------------.
#' @param x file
#' @return object
# -----------------------------------------------------------------------------.
load_description <- function(x, ...) {
  txtdef2obj(readLines(x), ...)
}

# LT_Workspace #################################################################

setClass("LT_Workspace", contains = "LT_Description")
# a <- new("LT_Workspace")
