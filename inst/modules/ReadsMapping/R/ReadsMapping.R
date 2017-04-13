# =============================================================================.
#
# -----------------------------------------------------------------------------.
availableMappingIndexes <- function(midx) {

  host <- Sys.info()["nodename"]
  midx <- midx[midx$hostname == host,]

  txt <- midx$sequence_path
  chk <- file.exists(txt)
  midx$sequence_available <- chk

  txt <- paste0(midx$sequence_path, ".gz")
  tst <- file.exists(txt)
  midx$sequence_path[tst & ! chk] <- txt[tst & ! chk]
  chk <- chk | tst
  midx$sequence_available <- chk

  midx$index_available <- file.exists(dirname(midx$index_path))
  midx
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
suggestMappingIndexes <- function(ann, midx, cmd) {

  ann$mapping_index <- ""
  x <- ann$organism_ch1
  for(i in 1:length(x)) {
    r <- GenomeInfoDb::genomeBuilds(x[i], style = "UCSC")
    for(j in nrow(r):1) {
      asm <- as.character(r[j, 3])
      chk <- with(midx, ucscID == asm & mapper == cmd)
      if(sum(chk) > 1) warning("multiple mapping indexes available")
      if(sum(chk) > 0) {
        ann$mapping_index[i] <- midx$identifier[which(chk)[1]]
        break
      }
    }
  }
  ann
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
validateMappingCommands <- function(x, mappers) {

  # Test if mapper is installed
  chk <- sapply(paste0("type ", x, "&>/dev/null"), system, intern = T)
  chk <- grepl(" not found$", chk, perl = T)
  if(any(chk)) {
    stop("mapper not installed ", paste(x[chk], collapse = ", "), call. = F)
  }

  # Test if mapper is supported
  chk <- x %in% mappers$command
  if(! all(chk)) {
    stop("mapper not supported ", paste(x[! chk], collapse = ", "), call. = F)
  }
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
validateMappingIndexes <- function(x, midx, cmd) {

  # Test if identifier is defined
  chk <- x %in% midx$identifier
  if(! all(chk)){
    stop("undefined index ", paste(x[! chk], collapse = ", "), call. = F)
  }

  # Test if mapping index is available
  chk <- dplyr::filter(midx, mapper == cmd & identifier %in% x)$index_available
  if(! all(chk)) {
    stop("index not available ", paste(x[! chk], collapse = ", "), call. = F)
  }
}
