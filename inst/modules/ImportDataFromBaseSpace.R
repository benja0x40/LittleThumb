# USAGE ########################################################################

# 1. Import a complete BaseSpace project
# - ---------------------------------- -
#
# $ HTS.ImportDataFromBaseSpace.R MyProjectName
#
# 2. Import a subset of samples from a BaseSpace project
# - -------------------------------------------------- -
#
# $ HTS.ImportDataFromGEO.R -q MyProjectName MySampleId1 MySampleId2
#
# 3. Import samples corresponding to a predefined dataset
# - --------------------------------------------------- -
#
# This can be used for instance to retreive missing samples after an incomplete
# importation from BaseSpace
# $ HTS.ImportDataFromGEO.R -d _METADATA_/BaseSpace.job.txt

# CONFIGURATION ################################################################

# =============================================================================.
# Internal configuration
# -----------------------------------------------------------------------------.
DTSFILE   <- paste0(ANNDIR, "/BaseSpace.", JOBID, ".txt") # dataset description
# =============================================================================.
# Define user options
# -----------------------------------------------------------------------------.
# Default values
BASEMOUNT  <- "~/BaseSpace"
INPDIR     <- NA
OUTDIR     <- "_RAWREADS_"
# -----------------------------------------------------------------------------.
option_list <- list(
  # Task specific options
  make_option(
    c("-q", "--query"),
    action  = "store", type = "character", default = "",
    help    = "BaseSpace project identifier",
    metavar = "PROJECT"
  ),
  make_option(
    c("-d", "--direct"),
    action  = "store_true", default = FALSE,
    help    = "Use a predefined dataset file instead of a project identifier"
  ),
  make_option(
    c("-f", "--filter"),
    action  = "store", type = "character", default = "",
    help    = "meta data filters to select a subset of samples",
    metavar = "KEY=VALUE,..."
  ),
  make_option(
    c("-m", "--mount"),
    action  = "store", type = "character", default = BASEMOUNT,
    help    = "path used to mount BaseSpace access",
    metavar = "DIR"
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
  OptionParser(option_list = option_list), args = JOBARGS,
  positional_arguments = TRUE
)
# print_options(opt$options, opt$args, lbl = "Query")
# -----------------------------------------------------------------------------.
if(! basespace_available(opt$options$mount)) {
  stop("BaseSpace file system not mounted properly")
}
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
# BaseSpace project identifier provided
if(! opt$options$direct) {
  if(length(opt$options$query) == 0) {
    stop("missing BaseSpace project identifier")
  }
  if(! opt$options$query %in% basespace_projects(opt$options$mount)) {
    stop("BaseSpace project not found")
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
BASEMOUNT <- opt$options$mount
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

# START LOGFILE ################################################################

# =============================================================================.
# Create output directory if necessary and redirect outputs to log file
# -----------------------------------------------------------------------------.
LOGFILE <- log_file(paste0(ROOTDIR, "/", LOGDIR))
sink(LOGFILE, split = T)
sink(LOGFILE, type = "message")

# GET METADATA #################################################################

# =============================================================================.
# Use BaseSpace project identifier (default)
# -----------------------------------------------------------------------------.
if(! UPDATE) {
  # ===========================================================================.
  # Retrieve meta data of BaseSpace samples
  # ---------------------------------------------------------------------------.
  ann <- basespace_dataset(BASEMOUNT, QUERY)
  # ---------------------------------------------------------------------------.
  # Filter samples based on identifiers
  ids <- ann$SampleId
  if(length(SPLIDS) > 0) ids <- intersect(ids, SPLIDS)
  if(length(ids) == 0) stop("BaseSpace samples not found")
  # ---------------------------------------------------------------------------.
  # Filter samples based on meta data
  if(SPLFILTER != "") ann <- filter_samples(ann, SPLFILTER)
  if(nrow(ann) == 0) stop("meta data filter resulted in empty selection")
  # ---------------------------------------------------------------------------.
  if(VERBOSE) {
    cat("Samples:\n")
    cat(paste0("  ", paste(ids, collapse = " "), "\n"))
  }
  # ===========================================================================.
  # Define names of raw read files
  # ---------------------------------------------------------------------------.
  ann$fastq_file <- paste0(QUERY, ".", ann$SampleId, ".fastq.gz")
  # ---------------------------------------------------------------------------.
  chk <- file.exists(paste0(ROOTDIR, "/", OUTDIR, "/", ann$fastq_file))
  if(any(chk)) {
    stop("identifiers of samples to be imported conflict with existant ones")
  }
}

# =============================================================================.
# Use a predefined dataset
# -----------------------------------------------------------------------------.
if(UPDATE) {
  # ---------------------------------------------------------------------------.
  # Load metadata of the predefined dataset
  ann <- read.delim(QUERY, stringsAsFactors = F)
  if(! all(c("SampleId", "basemount.path", "fastq_file") %in% colnames(ann))) {
    stop("incorrect dataset definition table")
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
  # Filter samples based on sample identifiers
  if(length(SPLIDS) > 0) ann <- ann[ann$SampleId %in% SPLIDS, ]
  if(nrow(ann) == 0) stop("sample identifiers resulted in empty selection")
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
    cat(paste0("  ", paste(ann$SampleId, collapse = " "), "\n"))
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
# Download fastq files from BaseSpace using rsync
# -----------------------------------------------------------------------------.
cmd <- with(
  ann,
  matrix(
    c(
      paste0("mkdir -p ", SampleId),
      paste0("rsync ", basemount.path, "/*.fastq.gz ", SampleId, "/")
    ),
    nrow = nrow(ann)
  )
)
# -----------------------------------------------------------------------------.
STATUS <- c(execute(cmd[, 1]), STATUS)
msg_command(cmd[, 1], "Create sample folders", status = STATUS[1])
# -----------------------------------------------------------------------------.
# Use several attempts to complete all downloads successfully
STATUS <- c(F, STATUS)
for(i in 1:MAXTRIES) {
  STATUS[1] <- execute(cmd[, 2])
  if(STATUS[1]) break
  Sys.sleep(TRYDELAY)
}
msg_command(cmd[, 2], "Download fastq files from BaseSpace", status = STATUS[1])
# =============================================================================.
# Concatenate and cleanup fastq files
# -----------------------------------------------------------------------------.
cmd <- with(
  # NOTE: using zcat+gzip is super slow and not necessary
  # ann, paste0("zcat $(ls -1 ", SampleId, "/*.fastq.gz) | gzip > ", fastq_file)
  ann, paste0("cat $(ls -1 ", SampleId, "/*.fastq.gz) > ", fastq_file)
)
STATUS <- c(execute(cmd), STATUS)
msg_command(cmd, "Concatenate fastq files", status = STATUS[1])
# -----------------------------------------------------------------------------.
chk <- file.info(ann$fastq_file)$size
if(any(is.na(chk)) | ! all(chk > 0)) stop("failed to concatenate fastq files")
# -----------------------------------------------------------------------------.
cmd <-  paste0("rm -Rf ", ann$SampleId)
STATUS <- c(execute(cmd), STATUS)
msg_command(cmd, "Delete original downloads", status = STATUS[1])
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
