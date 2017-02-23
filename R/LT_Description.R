# =============================================================================.
#
# -----------------------------------------------------------------------------.
lt_id <- function(tag = "") {
  paste0(
    format(Sys.time(), "%d.%m.%Y_%H.%M.%S_"),
    basename(tempfile(pattern = tag))
  )
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
setGeneric("r2json", function(obj, pretty) { standardGeneric("r2json") })
# -----------------------------------------------------------------------------.
setMethod(
  "r2json", signature(obj = "list"),
  function(obj) {
    txt <- toJSON(obj)
    txt <- prettify(txt, indent = 2)
    txt <- unlist(str_split(txt, "\n"))
    txt
  }
)
# -----------------------------------------------------------------------------.
setMethod(
  "r2json", signature(obj = "SimpleList"),
  function(obj) { r2json(as.list(obj)) }
)
# -----------------------------------------------------------------------------.
json2r <- function(x, Class = "list", ...) {
  as(fromJSON(x, ...), Class)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
setClass(
  "LT_Description",
  representation = representation(
    properties = "list", df = "DataFrame"
  ),
  prototype = prototype(
    properties = list(lt_id  = lt_id(), name = ""), df = DataFrame()
  )
)
# =============================================================================.
#
# -----------------------------------------------------------------------------.
obj2txt <- function(obj, ...) {
  txt <- textConnection("tmp", open = "w", local = T)
  rn <- ! is.null(rownames(obj@df))
  write.table(obj@df, txt, quote = F, sep = "\t", row.names = rn, col.names = T)
  txt <- c(
    str_pad("# ", pad = "=", width = 80, side = "right"),
    paste0("# Class = ", as.character(class(obj))),
    # str_pad("# ", pad = "-", width = 80, side = "right"),
    paste0("# ", str_pad(" jsonlite data ////", pad = "/", width = 78)),
    paste0("# ", r2json(obj@properties)),
    str_pad("# ", pad = "/", width = 80, side = "right"),
    paste0("# tabular data", ifelse(rn, " (with row names in the unnamed 1st column)", "")),
    str_pad("# ", pad = "=", width = 80, side = "right"),
    tmp
  )
  txt <- txt[! grepl("^# *$", txt, perl = T)]
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
txt2obj <- function(txt, obj = NULL, ...) {
  rex <- "^# *"
  chk <- grepl(rex, txt, perl = T)
  tbl <- paste(txt[! chk], collapse = "\n")
  txt <- gsub(rex, "", txt[chk], perl = T)

  rex <- "^(/{4,})"
  idx <- which(grepl(rex, txt, perl = T))
  idx <- (idx[1] + 1):(idx[2] - 1)
  prp <- txt[idx]
  txt <- txt[- idx]

  tbl <- read.delim(textConnection(tbl), header = T, stringsAsFactors = F)
  tbl <- as(tbl, "DataFrame")

  prp <- json2r(paste(prp, collapse = "\n"), ...)

  new("LT_Description", properties = prp, df = tbl)
}
# =============================================================================.
# TODO: use readr
# -----------------------------------------------------------------------------.
save_description <- function(obj, x) {
  writeLines(obj2txt(obj), x)
}
# =============================================================================.
# TODO: use readr
# -----------------------------------------------------------------------------.
load_description <- function(x, ...) {
  txt2obj(readLines(x), ...)
}

#
#
#
#
# setMethod(
#   "properties",
#   signature = signature(obj = "LT_Properties", e2 = "list"),
#   definition = function(object) {
#     nrow(object@df)
#   }
# )
#
# setGeneric(
#   "properties<-", function(obj) { standardGeneric("properties<-") }
# )
# setMethod(
#   "+",
#   signature = signature(e1 = "LT_Properties", e2 = "list"),
#   definition = function(object) {
#     nrow(object@df)
#   }
# )
# setClass(
#   "LT_Description",
#   representation = representation(
#     properties = "list", df = "DataFrame"
#   ),
#   prototype = prototype(
#     properties = list(lt_id  = lt_id()), df = DataFrame()
#   )
# )
# setGeneric(
#   "properties<-", function(object) { standardGeneric("properties<-") }
# )
#
# setGeneric(
#   "Nbr", function(object) {
#   standardGeneric("sides")
# })
# setMethod(
#   "len",
#   signature = signature(object = "LT_Description"),
#   definition = function(object) {
#     nrow(object@df)
#   }
# )
#
# x <- new(
#   "LT_Description",
#   df = DataFrame(
#     file = paste0("sample_", 1:5)
#   )
# )
#
#
# setMethod(
#   "saveMDTxt",
#   signature = c(object = "LT_MetaData")
#           c(x = "data.frame", y = "data.frame"),
#           function(x, y) {
#             unique(rbind(x, y))
#           }
# )

