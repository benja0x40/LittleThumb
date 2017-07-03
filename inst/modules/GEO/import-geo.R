# USAGE ########################################################################

# 1. Import a complete GEO series
# - --------------------------- -
#
# Automatically download the most up-to-date GEO/SRA database.
# It is recommended to keep a copy of the GEO/SRA database file,
# named SRAmetadb_%d.%m.%Y.sqlite by default,
# and to specify this file for subsequent data importations using
# HTS.ImportDataFromGEO.R, as shown in the next example.
# $ HTS.ImportDataFromGEO.R GSE63195
#
# Use an already downloaded GEO/SRA database.
# $ HTS.ImportDataFromGEO.R -m SRAmetadb_04.01.2016.sqlite GSE63195
#
# 2. Import a subset of samples from a GEO series
# - ------------------------------------------- -
#
# Provide the GEO accession number of each sample
# $ HTS.ImportDataFromGEO.R -q GSE63195 GSM1543688 GSM1543689
#
# Provide meta data filters to select samples
# $ HTS.ImportDataFromGEO.R -f "library_strategy=ChIP-Seq" GSE63195
#
# 3. Import samples corresponding to a predefined dataset
# - --------------------------------------------------- -
#
# This can be used for instance to retreive missing samples after an incomplete
# importation from GEO
# $ HTS.ImportDataFromGEO.R -d _METADATA_/GEO.SRA.job.txt

# CONFIGURATION ################################################################

# =============================================================================.
# Define user options
# -----------------------------------------------------------------------------.
# Default values
SRADBFILE <- paste0("SRAmetadb_", TODAY, ".sqlite")
INPDIR    <- NA
OUTDIR    <- "_RAWREADS_"
# -----------------------------------------------------------------------------.
option_list <- list(
  # Task specific options
  make_option(
    c("-q", "--query"),
    action  = "store", type = "character", default = "",
    help    = "GEO series accession number",
    metavar = "GEOID"
  ),
  make_option(
    c("-d", "--direct"),
    action  = "store_true", default = FALSE,
    help    = "Use a predefined dataset file instead of a GEO accession number"
  ),
  make_option(
    c("-f", "--filter"),
    action  = "store", type = "character", default = "",
    help    = "meta data filters to select a subset of samples",
    metavar = "KEY=VALUE,..."
  ),
  make_option(
    c("-m", "--metadb"),
    action  = "store", type = "character", default = SRADBFILE,
    help    = "SRA database (automatically downloaded if missing)",
    metavar = "FILE"
  ),
  # Standard options
  make_option(
    c("-t", "--threads"),
    action  = "store", type = "integer", default = THREADSNBR,
    help    = "maximum number of CPU threads to use (default = 4)",
    metavar = "INTEGER"
  ),
#  make_option(
#    c("-i", "--input"),
#    action  = "store", type = "character", default = INPDIR,
#    help    = paste0("input folder (default = ", INPDIR, ")"),
#    metavar = "PATH"
#  ),
  make_option(
    c("-o", "--output"),
    action  = "store", type = "character", default = OUTDIR,
    help    = paste0("output folder (default = ", OUTDIR, ")"),
    metavar = "PATH"
  ),
  make_option(
    c("-r", "--root"),
    action  = "store", type = "character", default = ROOTDIR,
    help    = paste0("root path (default = ", ROOTDIR, ")"),
    metavar = "PATH"
  ),
  make_option(
    c("-e", "--execute"),
    action  = "store_true", default = ! ASK2RUN,
    help    = "disable asking for confirmation"
  ),
  make_option(
    c("-s", "--silent"),
    action  = "store_true", default = ! VERBOSE,
    help    = "disable reporting status informations"
  )
)
# =============================================================================.
# Parse and verify command line options
# -----------------------------------------------------------------------------.
cat("\n")
opt <- parse_args(
  OptionParser(option_list = option_list), args = cmd_args(-1, as.string = F),
  positional_arguments = TRUE
)
# -----------------------------------------------------------------------------.
# print_options(opt$options, opt$args, lbl = "Query")
# -----------------------------------------------------------------------------.
if(opt$options$query == "") {
  opt$options$query <- opt$args
  opt$args <- NULL
}
# Predefined dataset provided
if(opt$options$direct) {
  if(length(opt$options$query) == 0) {
    stop("missing path of the dataset definition file")
  }
  if(! file.exists(opt$options$query)) {
    stop("dataset definition file not found")
  }
}
# GEO accession number provided
if(! opt$options$direct) {
  if(length(opt$options$query) == 0) {
    stop("missing GEO series accession number")
  }
  if(! grepl("^GSE[0-9]+$", opt$options$query, perl = T)) {
    stop("non-conform GEO series accession number")
  }
  if(! is.null(opt$args)) {
    if(! all(grepl("^GSM[0-9]+$", opt$args, perl = T))) {
      stop("non-conform GEO sample accession numbers")
    }
  }
  if(SRADBFILE != opt$options$metadb & ! file.exists(opt$options$metadb)) {
    stop("metadb file not found")
  }
}
# =============================================================================.
# Extract option values
# -----------------------------------------------------------------------------.
ASK2RUN <- ! opt$options$execute
VERBOSE <- ! opt$options$silent
THREADSNBR <- opt$options$threads
ROOTDIR    <- opt$options$root
# INPDIR     <- opt$options$input
OUTDIR     <- opt$options$output
# -----------------------------------------------------------------------------.
SRADBFILE <- opt$options$metadb
QUERY     <- opt$options$query
UPDATE    <- opt$options$direct
SPLFILTER <- opt$options$filter
SPLIDS    <- opt$args
# -----------------------------------------------------------------------------.
# Display options and confirm execution
if(VERBOSE) print_options(opt$options, opt$args, lbl = "Query")
if(ASK2RUN) confirm_execution()
# -----------------------------------------------------------------------------.
# Cleanup
rm(opt, option_list)

# DEBUG VALUES #################################################################
# QUERY  <- "GSE63195"
# UPDATE <- F
# SPLFILTER <- NULL
# SPLIDS <- NULL
# SRADBFILE <- "/media/SSD512GB/tst1/SRAmetadb_07.04.2017.sqlite"

# START LOGFILE ################################################################

# =============================================================================.
# Create output directory if necessary and redirect outputs to log file
# -----------------------------------------------------------------------------.
LOGFILE <- log_file(paste0(ROOTDIR, "/", LOGDIR))
sink(LOGFILE, split = T)
sink(LOGFILE, type = "message")

# GET METADATA #################################################################

# =============================================================================.
# Use GEO accession numbers (default)
# -----------------------------------------------------------------------------.
if(! UPDATE) {
  # ===========================================================================.
  # Download SRA metadb if not available and open sqlite connexion
  # ---------------------------------------------------------------------------.
  if(! file.exists(SRADBFILE)) {
    if(VERBOSE) cat("Downloading GEO/SRA meta data...\n")
    getSRAdbFile(destfile = paste(SRADBFILE, ".gz", sep=""))
  }
  sra_dbcon <- dbConnect(dbDriver("SQLite"), SRADBFILE)

  # ===========================================================================.
  # Retrieve the GEO series
  # ---------------------------------------------------------------------------.
  gse <- getGEO(QUERY, GSEMatrix = F)
  ids <- Meta(gse)$sample_id
  # ---------------------------------------------------------------------------.
  # Filter samples based on GEO sample accession numbers
  if(length(SPLIDS) > 0) ids <- intersect(ids, SPLIDS)
  if(length(ids) == 0) {
    stop("GEO sample identifiers resulted in empty selection")
  }

  # ===========================================================================.
  # Retrieve meta data of GEO samples
  # ---------------------------------------------------------------------------.
  gmd <- geo_meta_data(ids, LOGFILE = LOGFILE)
  gsm <- gmd$gsm
  ann <- gmd$ann
  rm(gmd)
  # ---------------------------------------------------------------------------.
  # Filter samples based on meta data
  if(SPLFILTER != "") ann <- filter_samples(ann, SPLFILTER)
  if(nrow(ann) == 0) stop("meta data filter resulted in empty selection")
  # ---------------------------------------------------------------------------.
  # Retain only samples with sequencing data
  ann <- ann[grepl("^SRX", ann$srx, perl=T),]
  if(nrow(ann) == 0) stop("sequencing data not found")
  ids <- ann$gsm
  gsm <- gsm[ids]
  # ---------------------------------------------------------------------------.
  if(VERBOSE) {
    cat("Samples:\n")
    cat(paste0("  ", paste(ids, collapse = " "), "\n"))
  }

  # ===========================================================================.
  # Link GEO samples to SRA entries
  # ---------------------------------------------------------------------------.
  sra <- sraConvert(in_acc = ann$srx, sra_con = sra_dbcon)
  if(sum(ann$srx != sra$experiment)>0) {
    txt_out(x = "=", file = LOGFILE)
    txt_out("Non matching SRX in GEO and SRA entries", LOGFILE)
    txt_out(x = "-", LOGFILE)
    write.table(
      ann, file = LOGFILE,
      append = T, quote = F, sep = "\t", row.names = F, col.names = T
    )
    txt_out(x = "-", LOGFILE)
    write.table(
      sra, file = LOGFILE,
      append = T, quote = F, sep = "\t", row.names = F, col.names = T
    )
    txt_out(x = "-", LOGFILE)
    stop("importation failed (see logfile)" )
  }
  sra$geo.gse   <- Meta(gse)$geo_accession
  sra$geo.gsm   <- ann$gsm
  ids <- sra$geo.gsm
  sra$ftp.path <- ""
  for(lbl in ids) {
    i <- which(lbl == ids)
    j <- which(grepl("supplementary_file", names(Meta(gsm[[lbl]]))))
    j <- j[grepl("/sra/", Meta(gsm[[lbl]])[j])]
    sra$ftp.path[i] <- Meta(gsm[[lbl]])[[j]]
    sra$ftp.path[i] <- paste(
      sra$ftp.path[i], "/", sra$run[i], "/", sra$run[i], ".sra", sep=""
    )
  }
  chk <- gsub(".*/(SRX[0-9]+)/SRR.*", "\\1", sra$ftp.path, perl=T)
  if(sum(ann$srx != chk)>0) {
    txt_out(x = "=", file = LOGFILE)
    txt_out("Unable to build path for ftp download", file = LOGFILE)
    txt_out(x = "-", file = LOGFILE)
    txt_out(sra$ftp.path, sep = "\n", file = LOGFILE)
    txt_out(x = "-", file = LOGFILE)
    txt_out(chk, sep = "\n", file = LOGFILE)
    txt_out(x = "-", file = LOGFILE)
    txt_out(ann$srx, sep = "\n", file = LOGFILE)
    txt_out(x = "-", file = LOGFILE)
    stop("importation failed (see logfile)" )
  }
  sra$geo.title <- ann$name
  sra <- cbind(sra, ann[,4:ncol(ann)])
  ann <- sra
  # ===========================================================================.
  # Define names of raw read files
  # ---------------------------------------------------------------------------.
  ann$fastq_file <- paste0(ann$run, ".fastq.gz")
  # ---------------------------------------------------------------------------.
  chk <- file.exists(paste0(ROOTDIR, "/", OUTDIR, "/", ann$fastq_file))
  if(any(chk)) {
    stop("identifiers of samples to be imported conflict with existant ones")
  }
  # ---------------------------------------------------------------------------.
  # dataset description
  DTSFILE  <- paste0(ANNDIR, "/", cmd_args(1), ".", QUERY, ".", JOBID, ".txt")
}

# =============================================================================.
# Use a predefined dataset
# -----------------------------------------------------------------------------.
if(UPDATE) {
  # ---------------------------------------------------------------------------.
  # Load metadata of the predefined dataset
  ann <- read.delim(QUERY, stringsAsFactors = F)
  if(! all(c("geo.gsm", "ftp.path", "fastq_file") %in% colnames(ann))) {
    stop("incorrect dataset description file")
  }
  # ---------------------------------------------------------------------------.
  # Skip samples for which raw reads (fastq.gz files) are already available
  chk <- paste0(ROOTDIR, "/",  OUTDIR, "/", ann$fastq_file)
  ann <- ann[! file.exists(chk), ]
  if(nrow(ann) == 0) {
    message("The complete dataset seems already available")
    message("To re-import specific samples, delete the corresponding fastq.gz files and run the command once again")
    q(save = "no", status = 0)
  }
  # ---------------------------------------------------------------------------.
  # Filter samples based on GEO sample accession numbers
  if(length(SPLIDS) > 0) ann <- ann[ann$geo.gsm %in% SPLIDS, ]
  if(nrow(ann) == 0) stop("GEO sample identifiers resulted in empty selection")
  # ---------------------------------------------------------------------------.
  # Filter samples based on meta data
  if(SPLFILTER != "") ann <- filter_samples(ann, SPLFILTER)
  if(nrow(ann) == 0) stop("meta data filter resulted in empty selection")
  # ---------------------------------------------------------------------------.
  # Make a copy of the dataset definition file if necessary (for consistency)
  DTSFILE <- paste0(ROOTDIR, "/", ANNDIR, "/", basename(QUERY))
  if(! file.exists(DTSFILE)) {
    file.copy(QUERY, DTSFILE)
  }
  DTSFILE <- paste0(ANNDIR, "/", basename(DTSFILE))
  # ---------------------------------------------------------------------------.
  if(VERBOSE) {
    cat("Samples:\n")
    cat(paste0("  ", paste(ann$geo.gsm, collapse = " "), "\n"))
  }
}

# IMPORT DATA ##################################################################

# =============================================================================.
setwd(ROOTDIR)
# -----------------------------------------------------------------------------.
CMDFILE <- file(CMDFILE, open = "a")
msg_header(
  paste(JOBID, "|", format(STARTTIME, "%d.%m.%Y %H:%M:%S")), file = CMDFILE
)
msg_command()
# -----------------------------------------------------------------------------.
cmd <- paste("mkdir -p", c(LOGDIR, ANNDIR, OUTDIR))
STATUS <- execute(cmd)
msg_command(cmd, "Make output directories", chrono = F, status = STATUS[1])
# -----------------------------------------------------------------------------.
cmd <- paste(c("mkdir -p", "cd"), JOBID)
STATUS <- c(execute(cmd), STATUS)
msg_command(
  cmd, "Use temporary directory for current job", chrono = F, status = STATUS[1]
)
# -----------------------------------------------------------------------------.
setwd(JOBID)
# =============================================================================.
# EBI ENA (future option, not yet working)
# cmd <- paste0("wget -c ", ebi_ena_url(ann))
# -----------------------------------------------------------------------------.
# NCBI GEO
cmd <- paste("wget -c", ann$ftp.path)
# -----------------------------------------------------------------------------.
# Use several attempts to complete all downloads successfully
STATUS <- c(F, STATUS)
for(i in 1:MAXTRIES) {
  STATUS[1] <- execute(cmd)
  if(STATUS[1]) break
  Sys.sleep(TRYDELAY)
}
msg_command(cmd, "Download data from NCBI", status = STATUS[1])
# =============================================================================.
# Convert sra to compressed fastq files
# -----------------------------------------------------------------------------.
cmd <- c("fastq-dump *.sra", "rm *.sra")
STATUS <- c(execute(cmd), STATUS)
msg_command(cmd, "Convert sra to fastq files", status = STATUS[1])
# -----------------------------------------------------------------------------.
cmd <- "gzip *.fastq"
STATUS <- c(execute(cmd), STATUS)
msg_command(cmd, "Compress fastq files", status = STATUS[1])
# =============================================================================.
setwd("..")
# -----------------------------------------------------------------------------.
cmd <- c(
  "cd ..",
  paste0(   "mv ", JOBID, "/*.fastq.gz ", OUTDIR, "/"),
  paste0("rmdir ", JOBID)
)
STATUS <- c(execute(cmd), STATUS)
msg_command(cmd, "Move downloaded data and cleanup", status = STATUS[1])
# =============================================================================.
# Verify fastq files
# -----------------------------------------------------------------------------.
# *** TODO ***
# STATUS <- c(
#   all(file.exists( paste0(OUTDIR, "/", ann$fastq_file))), STATUS
# )
# cmd <- paste0("gunzip -c ", OUTDIR, "/", ann$fastq_file, " | wc -l")
# nbr <- sapply(cmd, system)
# =============================================================================.
# Save meta data of imported samples
# -----------------------------------------------------------------------------.
if(! UPDATE) {
  write.table(
    ann, DTSFILE, quote = F, sep = "\t", row.names = F, col.names = T
  )
}
