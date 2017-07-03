# =============================================================================.
#
# -----------------------------------------------------------------------------.
geo_characteristics <- function(gsm, ids, common = T) {
  rex <- "^([^\\:]+): (.*)$"
  ann <- data.frame(row.names = ids, stringsAsFactors = F)
  for(lbl in ids) {
    lst <- names(gsm[[lbl]]@header)
    lst <- lst[grepl("characteristics", lst)]
    i <- 0
    for(elm in lst) {
      i <- i + 1
      v <- gsm[[lbl]]@header[[elm]]
      k <- make.names(gsub(rex, "\\1", v, perl = T), unique = T)
      if(length(lst) > 1) k <- paste0(k, ".ch", i)
      v <- gsub(rex, "\\2", v, perl = T)
      chk <- ! k %in% colnames(ann)
      ann <- cbind(
        ann, matrix("", nrow(ann), sum(chk), dimnames = list(NULL, k[chk])),
        stringsAsFactors = F
      )
      ann[lbl, k] <- v
    }
  }
  if(common) {
    chk <- ! sapply(ann, function(x) any(x == "" | is.na(x)))
    ann <- ann[, chk]
  }
  ann
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
geo_meta_data <- function(ids, characteristics = T, remove_ch = T, LOGFILE = stdout(), ...) {

  # regular expression defining meta data to be imported
  rex <- c(
    "instrument", "platform",
    "library", "source", "molecule",
    "organism", "taxid",
    "genotype", "tissue", "stage", "cell", "treatment", "time", "antibody",
    "characteristics"
  )
  rex <- paste0("(", paste(rex, collapse = ")|(", sep = ""), ")")

  # regular expression defining meta data to be skipped
  skp <- "(contact)|(protocol)|(characteristics)"

  # regular expression for dual channel tags (legacy of microarray annotations)
  ch1 <- "(\\.|_)ch1$"
  ch2 <- "(\\.|_)ch2$"

  gsm <- list()
  ann <- data.frame(
    name = ids,
    gsm  = ids,
    srx  = ids,
    stringsAsFactors = F
  )
  row.names(ann) <- ids

  txt_out(x = "=", file = LOGFILE)
  txt_out("Retrieved from Bioconductor SRADB", LOGFILE)
  txt_out(x = "-", LOGFILE)
  write.table(
    ann, file = LOGFILE,
    append = T, quote = F, sep = "\t", row.names = F, col.names = T
  )

  for(lbl in ids) {
    gsm[[lbl]] <- GEOquery::getGEO(lbl)
    ann[lbl, "name"] <- gsub(" ", "_", gsm[[lbl]]@header$title)
    if("relation" %in% names(gsm[[lbl]]@header)) {
      s.i <- which(
        grepl(".*sra\\?term=(SRX[0-9]+)$", gsm[[lbl]]@header$relation, perl = T)
      )
      ann[lbl,"srx"]  <- gsub(
        ".*sra\\?term=(SRX[0-9]+)$", "\\1", gsm[[lbl]]@header$relation[s.i],
        perl = T
      )
    }
    lst <- names(gsm[[lbl]]@header)

    txt_out("+", lbl, ": found", x = "-", LOGFILE)
    txt_out(lst, sep = "\n", file = LOGFILE)

    lst <- lst[grepl(rex, lst)]
    lst <- lst[! grepl(skp, lst)]

    txt_out("+", lbl, ": filtered", x = "-", LOGFILE)
    txt_out(lst, sep = "\n", file = LOGFILE)
    txt_out(x = "-", LOGFILE)

    for(elm in lst) {
      if(! elm %in% names(ann)) {
        ann[[elm]] <- ""
      }
      ann[lbl, elm] <- gsm[[lbl]]@header[elm]
    }
  }
  if(characteristics) {
    ann <- cbind(
      ann, geo_characteristics(gsm, ids, ...), stringsAsFactors = F
    )
  }

  txt_out(x = "=", file = LOGFILE)
  txt_out("Imported annotations", LOGFILE)
  txt_out(x = "-", LOGFILE)
  write.table(
    ann, file = LOGFILE,
    append = T, quote = F, sep = "\t", row.names = F, col.names = T
  )

  if(remove_ch) {
    if(any(grepl(ch2, colnames(ann), perl = T))) {
      stop("found multiple channel information in meta data (i.e. _ch1, _ch2)")
    } else {
      colnames(ann) <- gsub(ch1, "", colnames(ann), perl =T)
    }
  }
  list(gsm = gsm, ann = ann)
}
