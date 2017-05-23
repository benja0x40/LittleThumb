# PACKAGES #####################################################################

library(GenomicAlignments)
library(GenomicFIO)
library(LittleThumb)
library(RCNorm)

# FUNCTIONS ####################################################################

# TESTS ########################################################################

openLittleThumb()

# =============================================================================.
root_path <- "~/Desktop/LT_TESTS/"
wks <- "TAIR10"
# -----------------------------------------------------------------------------.
refresh <- F
# -----------------------------------------------------------------------------.
if(refresh & wks %in% list_workspaces()) {
  delete_workspace(wks, ask = F)
  resetLittleThumb(ask = F)
  rm(list = setdiff(ls(all.names = T), c("wks", "root_path")))
  openLittleThumb()
} else {
  rm(refresh)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
if(! wks %in% list_workspaces()) {
  define_workspace(wks, make_path(root_path, wks))
  create_workspace(wks)
}
open_workspace(wks)
# =============================================================================.
# This dataset should be generate by module ReadMapping/map-reads.R
# -----------------------------------------------------------------------------.
dts <- "bam"
# -----------------------------------------------------------------------------.
if(! dts %in% list_datasets()) {
  data_path <- "~/Desktop/LT_TESTS/systemPipeRdata/inst/extdata/"
  # Load annotations
  ann <- read.delim(
    make_path(data_path, "param/targets.txt"), comment.char = "#",
    stringsAsFactors = F
  )
  lst <- paste0(data_path, "bam/", basename(ann$FileName), ".hisat.bam")
  # checklist(file.exists(lst), lst)
  # Create dataset
  create_dataset(
    wks, name = dts, path = "MAPPED_READS/bam",
    files = lst, annotations = ann,
    id_column = "SampleLong" # , file_columns = "file"
  )
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
dts <- "mapped_reads"
# -----------------------------------------------------------------------------.
if(! dts %in% list_datasets()) {
  # Load mapped reads dataset (bam)
  load_data(wks, dataset = "bam", reader = readGAlignments)
  # load_data(
  #   wks, dataset = dts, reader = readGAlignments,
  #   param = ScanBamParam(what = c("qual", "mapq"))
  # )
  # Make new dataset for mapped reads as rdata
  lst <- paste0(gsub(".fastq.gz", "", basename(ann$FileName)), ".rdata")
  clone_dataset(
    wks, "bam", name = dts, path = "MAPPED_READS/rdata", files = lst
  )
  save_data(wks, dataset = dts, writer = saveRDS)
}
if(! dts %in% ls(pos = TAIR10)) {
  load_data(wks, dataset = dts, reader = readRDS)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
if(! loadObj(TAIR10_genes_grg, path = "~/Desktop/LT_TESTS/extra")) {
  gtf <- importGTF("~/Desktop/LT_TESTS/extra/TAIR10_GFF3_genes.gff.txt")
  gtf <- gtf[gtf$feature == "gene", ]
  TAIR10_genes_grg <- importedGTF2GRanges(gtf, with.attributes = F)
  saveObj(TAIR10_genes_grg, path = "~/Desktop/LT_TESTS/extra")
  rm(gtf)
}

CNT <- makeReadCountMatrix(
  aln = TAIR10$mapped_reads, grg = TAIR10_genes_grg, ignore.strand = T
)
colnames(CNT) <- names(TAIR10$mapped_reads)
CNT <- log2(ditherCounts(CNT))
CNT <- CNT[finiteValues(CNT), ]

# coverage()

library(parallel)

detectCores()

# For asynchronous execution with basic notification
job <- mcparallel({
  Sys.sleep(5)
  system("echo '[LittleThumb] job done'")
})

# For tracking of file modifications
file.info("DESCRIPTION")     # primary test
system("md5 -q DESCRIPTION") # secondary test

print(mccollect(job)[[1]])


