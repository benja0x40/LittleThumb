# =============================================================================.
# STUPID: a shell session started and terminated for each call of system()
# -----------------------------------------------------------------------------.
# basespace_mount <- function(mnt) {
#   if(! file.exists(mnt)) {
#     if(! execute(paste0('mkdir -p "', mnt, '"'))) {
#       stop("unusable mount path ", mnt)
#     }
#   }
#   if(! execute(paste0('basemount "', mnt, '"'))) {
#     stop("basespace mount command failed")
#   }
#   T
# }
# =============================================================================.
# STUPID: a shell session started and terminated for each call of system()
# -----------------------------------------------------------------------------.
# basespace_unmount <- function(mnt) {
#   if(! file.exists(mnt)) {
#     stop("non-existent mount path ", mnt)
#   }
#   if(! execute(paste0('basemount --unmount "', mnt, '"'))) {
#     stop("basespace unmount command failed")
#   }
#   T
# }
# =============================================================================.
#
# -----------------------------------------------------------------------------.
basespace_available <- function(mnt) {
  file.exists(paste0(mnt, "/Projects"))
}
# =============================================================================.
#' basespace_projects
# -----------------------------------------------------------------------------.
#' @param mnt BaseSpace mount path
#' @return basespace_projects returns a character vector of project names
# -----------------------------------------------------------------------------.
basespace_projects <- function(mnt) {
  dir(paste0(mnt, "/Projects"))
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
basespace_properties <- function(x) {
  rex <- "^([^:]+): (.*)$"
  x <- readLines(x)
  k <- gsub(rex, "\\1", x)
  v <- gsub(rex, "\\2", x)
  names(v) <- make.names(k)
  v
}
# =============================================================================.
#' basespace_dataset
# -----------------------------------------------------------------------------.
#' @param mnt BaseSpace mount path
#' @param prj project name
#' @return basespace_dataset returns a data.frame of sample properties
# -----------------------------------------------------------------------------.
basespace_dataset <- function(mnt, prj) {
  fpath <- paste0(mnt, "/Projects/", prj, "/Samples")
  flist <- paste0(fpath, "/", dir(fpath), "/SampleProperties")
  ann <- c()
  for(fname in flist) {
    ann <- rbind(ann, basespace_properties(fname))
  }
  ann <- data.frame(ann, stringsAsFactors = F)
  ann$project        <- prj
  ann$basemount.host <- LittleThumb::host_name()
  ann$basemount.path <- paste0(dirname(flist), "/Files")
  ann
}
