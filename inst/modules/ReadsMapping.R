# USAGE ########################################################################

# 1. Perform reads mapping of a complete dataset
# - ------------------------------------------ -
#
# $ HTS.ReadsMapping.R MyDatasetFile
#
# 2. Perform reads mapping of a subset of samples
# - ------------------------------------------- -
#
# Provide the GEO accession number of each sample
# $ HTS.ReadsMapping.R -q MyDatasetFile MySampleId1 MySampleId2
#
# Provide meta data filters to select samples
# $ HTS.ReadsMapping.R -f "library_strategy=ChIP-Seq" MyDatasetFile

# CONFIGURATION ################################################################

# =============================================================================.
# Internal configuration
# -----------------------------------------------------------------------------.
DTSFILE   <- paste0(ANNDIR, "/LittleThumb.", JOBID, ".txt") # dataset description
# -----------------------------------------------------------------------------.
# Supported mappers
MAPPERS <- data.frame(
  command = c("bowtie", "bowtie2", "bwa", "STAR"),
  website = c(
    bowtie  = "http://bowtie-bio.sourceforge.net",
    bowtie2 = "http://bowtie-bio.sourceforge.net/bowtie2",
    bwa     = "http://bio-bwa.sourceforge.net",
    STAR    = ""
  ),
  default.options = c(
    bowtie  = "--chunkmbs 256 -m 1 --strata --best",
    bowtie2 = "--sensitive",
    bwa     = "",
    STAR    = ""
  ),
  stringsAsFactors = F
)
# =============================================================================.
# Define user options
# -----------------------------------------------------------------------------.
# Default values
MAP_CMD <- "bowtie"
MAP_PAR <- ""
MAP_IDX <- "genomic_hg38,genomic_dm3" #
INPDIR     <- "_RAWREADS_"
OUTDIR     <- "_MAPPEDREADS_"
# -----------------------------------------------------------------------------.
option_list <- list(
  # Task specific options
  make_option(
    c("-q", "--query"),
    action  = "store", type = "character", default = "",
    help    = "dataset description file",
    metavar = "DATASET"
  ),
  make_option(
    c("-f", "--filter"),
    action  = "store", type = "character", default = "",
    help    = "meta data filters to select a subset of samples",
    metavar = "KEY=VALUE,..."
  ),
  make_option(
    c("-m", "--mapper"),
    action  = "store", type = "character", default = MAP_CMD,
    help    = "command name used to perform reads mapping",
    metavar = "COMMAND"
  ),
  make_option(
    c("-p", "--parameters"),
    action  = "store", type = "character", default = MAP_PAR,
    help    = "command options used to perform reads mapping",
    metavar = "OPTIONS"
  ),
  make_option(
    c("-x", "--index"),
    action  = "store", type = "character", default = MAP_IDX,
    help    = "precomputed sequence indexes for reads mapping",
    metavar = "IDENTIFIERS"
  ),
  # Standard options
  make_option(
    c("-t", "--threads"),
    action  = "store", type = "integer", default = THREADSNBR,
    help    = "maximum number of CPU threads to use (default = 4)",
    metavar = "INTEGER"
  ),
  make_option(
    c("-i", "--input"),
    action  = "store", type = "character", default = INPDIR,
    help    = paste0("input folder (default = ", INPDIR, ")"),
    metavar = "PATH"
  ),
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
if(opt$options$query == "") {
  opt$options$query <- opt$args
  opt$args <- NULL
}
# if(length(opt$options$query) == 0) {
#   stop("missing path of the dataset definition file")
# }
# if(! file.exists(opt$options$query)) {
#   stop("dataset definition file not found")
# }
# =============================================================================.
# Extract option values
# -----------------------------------------------------------------------------.
ASK2RUN <- ! opt$options$execute
VERBOSE <- ! opt$options$silent
THREADSNBR <- opt$options$threads
ROOTDIR    <- opt$options$root
INPDIR     <- opt$options$input
OUTDIR     <- opt$options$output
# -----------------------------------------------------------------------------.
QUERY     <- opt$options$query
SPLFILTER <- opt$options$filter
SPLIDS    <- opt$args
# -----------------------------------------------------------------------------.
# Display options and confirm execution
if(VERBOSE) print_options(opt$options, opt$args, lbl = "Query")
if(ASK2RUN) confirm_execution()
# -----------------------------------------------------------------------------.
# Cleanup
rm(opt, option_list)

# =============================================================================.
# Resolve dataset
# -----------------------------------------------------------------------------.
QUERY <- "_METADATA_/BaseSpace.job_3ce25f5f7a2f.txt"
QUERY <- "~/Dropbox/Workflow/BaseSpace.job_3ce25f5f7a2f.txt"
# ---------------------------------------------------------------------------.
dts_ann <- read.delim(QUERY, stringsAsFactors = F)
if(! all(c("fastq_file") %in% colnames(dts_ann))) {
  stop("incorrect dataset description file")
}
# ---------------------------------------------------------------------------.
# Filter samples based on sample identifiers
# if(length(SPLIDS) > 0) dts_ann <- dts_ann[dts_ann$geo.gsm %in% SPLIDS, ]
# if(nrow(dts_ann) == 0) stop("sample identifiers resulted in empty selection")
# ---------------------------------------------------------------------------.
# Filter samples based on meta data
if(SPLFILTER != "") dts_ann <- filter_samples(dts_ann, SPLFILTER)
if(nrow(dts_ann) == 0) stop("meta data filter resulted in empty selection")
# ---------------------------------------------------------------------------.
# Make a copy of the dataset definition file if necessary (for consistency)
# DTSFILE <- paste0(ROOTDIR, "/", ANNDIR, "/", basename(QUERY))
# if(! file.exists(DTSFILE)) {
#   file.copy(QUERY, DTSFILE)
# }
# DTSFILE <- paste0(ANNDIR, "/", basename(DTSFILE))

# =============================================================================.
# Resolve mapping command
# -----------------------------------------------------------------------------.
MAP_CMD <- unique(unlist(str_split(MAP_CMD, ",")))
MAP_CMD <- MAP_CMD[MAP_CMD != ""]
dts_ann <- md_set("mapping_command", dts_ann, values = MAP_CMD)

if(! md_tst("mapping_command", dts_ann)) {
  stop("missing mapping command")
}
for(lbl in md_lst("mapping_command", dts_ann)) {
  validateMappingCommands(dts_ann[[lbl]], MAPPERS)
}

# =============================================================================.
# Resolve mapping options
# -----------------------------------------------------------------------------.
MAP_PAR <- unique(unlist(str_split(MAP_PAR, ",")))
MAP_PAR <- paste0(" ", str_trim(MAP_PAR), " ")
MAP_PAR <- MAP_PAR[MAP_PAR != "  "]
dts_ann <- md_set("mapping_options", dts_ann, values = MAP_PAR)

# *** TODO *** Handle multiple commands

if(! md_tst("mapping_options", dts_ann)) {
  message("Using default mapping options")
  MAP_PAR <- paste0(" ", str_trim(MAPPERS[MAP_CMD, "default.options"]), " ")
  dts_ann$mapping_options <- MAP_PAR
}

# =============================================================================.
# Resolve mapping indexes
# -----------------------------------------------------------------------------.
MAP_IDX <- unique(unlist(str_split(MAP_IDX, ",")))
MAP_IDX <- MAP_IDX[MAP_IDX != ""]
dts_ann <- md_set("mapping_index", dts_ann, values = MAP_IDX)

if(! md_tst("mapping_index", dts_ann)) {
  dts_ann <- suggestMappingIndexes(dts_ann, MAPPING_INDEXES, MAP_CMD)
}

idx <- md_lst("mapping_index", dts_ann)
for(lbl in idx) {
  validateMappingIndexes(dts_ann[[lbl]], MAPPING_INDEXES, MAP_CMD)
}

MAPPING_INDEXES <- filter(MAPPING_INDEXES, mapper == MAP_CMD)
MAPPING_INDEXES <- filter(
  MAPPING_INDEXES, identifier %in% as.vector(as.matrix(dts_ann[, idx]))
)

# =============================================================================.
# Resolve fastq file paths
# -----------------------------------------------------------------------------.

# *** TODO *** make sure that dts_ann$fastq_file are not full paths

fastq_paths <- paste0(ROOTDIR, "/", INPDIR, "/", dts_ann$fastq_file)
if(! all(file.exists(fastq_paths))) stop("missing fastq files")

fastq_paths <- paste0("../", INPDIR, "/", dts_ann$fastq_file)
output_list <- gsub("\\.fastq.gz$", "", basename(dts_ann$fastq_file))

# =============================================================================.
# Build full commands
# -----------------------------------------------------------------------------.
cmd <- matrix("", nrow(dts_ann), md_nbr("mapping_index", dts_ann))
colnames(cmd) <- md_lst("mapping_index", dts_ann)

if(MAP_CMD == "bowtie") {
  for(idx in md_lst("mapping_index", dts_ann)) {
    MIDX <- match(dts_ann[[idx]], MAPPING_INDEXES$identifier)
    MIDX <- MAPPING_INDEXES$index_path[MIDX]
    cmd[, idx] <- paste0(
      "gunzip -c ", fastq_paths, " | bowtie -p ", THREADSNBR, MAP_PAR, "--sam ",
      MIDX, " /dev/stdin ", output_list, ".sam"
      # " --un ", output_list, ".unmapped.fastq"
    )

  }
}
if(MAP_CMD == "bowtie2") {
  # gunzip -c $FPATH | bowtie -p $CFG_CPU $CFG_OPT --sam "$GENOME_INDEX1" /dev/stdin "$MAPPED_READS1.sam" # --un "$MAPPED_READS1.unmapped.fastq"
  MIDX <- match(dts_ann$mapping_index, MAPPING_INDEXES$identifier)
  MIDX <- MAPPING_INDEXES$index_path[MIDX]
  cmd <- paste0(
    "gunzip -c ", fastq_paths, " | bowtie -p ", THREADSNBR, MAP_PAR, "--sam ", MIDX, " /dev/stdin "
  )
}
if(MAP_CMD == "STAR") {
}
