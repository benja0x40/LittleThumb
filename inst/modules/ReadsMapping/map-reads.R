# USAGE ########################################################################

# 1. Perform reads mapping of a complete dataset
# - ------------------------------------------ -
#
# $ littlethumb map-reads MyDatasetFile
#
# 2. Perform reads mapping of a subset of samples
# - ------------------------------------------- -
#
# Provide the GEO accession number of each sample
# $ littlethumb map-reads -q MyDatasetFile MySampleId1 MySampleId2
#
# Provide meta data filters to select samples
# $ littlethumb map-reads -f "library_strategy=ChIP-Seq" MyDatasetFile

# CONFIGURATION ################################################################

# =============================================================================.
# Supported mappers
# -----------------------------------------------------------------------------.
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
MAP_IDX <- ""
INPDIR  <- "_RAWREADS_"
OUTDIR  <- "_MAPPEDREADS_"
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
MAP_CMD   <- opt$options$mapper
MAP_PAR   <- opt$options$parameters
MAP_IDX   <- opt$options$index
# -----------------------------------------------------------------------------.
# Display options and confirm execution
if(VERBOSE) print_options(opt$options, opt$args, lbl = "Query")
if(ASK2RUN) confirm_execution()
# -----------------------------------------------------------------------------.
# Cleanup
rm(opt, option_list)

# DEBUG VALUES #################################################################
# QUERY <- "_ANNOTATIONS_/BaseSpace.job_3ce25f5f7a2f.txt"
# SPLFILTER <- ""
# SPLIDS <- ""
# -----------------------------------------------------------------------------.
# QUERY <- "_ANNOTATIONS_/import-basespace.ESC_BRD.LT20170414_1738_2e9e1a9a1ac1.txt"
# SPLFILTER <- ""
# SPLIDS <- ""
# MAP_CMD <- "bwa"
# MAP_IDX <- "genomic_dm6,genomic_mm10"

# PREPARE MAPPING ##############################################################

# =============================================================================.
# Resolve dataset
# -----------------------------------------------------------------------------.
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

MAPPING_INDEXES <- dplyr::filter(MAPPING_INDEXES, mapper == MAP_CMD)
MAPPING_INDEXES <- dplyr::filter(
  MAPPING_INDEXES, identifier %in% as.vector(as.matrix(dts_ann[, idx]))
)

# =============================================================================.
# Resolve fastq file paths
# -----------------------------------------------------------------------------.

# *** TODO *** make sure that dts_ann$fastq_file are not full paths

fastq_paths <- paste0(ROOTDIR, "/", INPDIR, "/", dts_ann$fastq_file)
if(! all(file.exists(fastq_paths))) stop("missing fastq files")

fastq_paths <- paste0(INPDIR, "/", dts_ann$fastq_file)
output_list <- gsub("\\.fastq.gz$", "", basename(dts_ann$fastq_file))

# =============================================================================.
# Output datasets
# -----------------------------------------------------------------------------.
n_cmd <- length(md_lst("mapping_command", dts_ann))
n_par <- length(md_lst("mapping_options", dts_ann))
n_idx <- length(md_lst("mapping_index", dts_ann))

col_lst <- c(
  md_lst("mapping_command", dts_ann),
  md_lst("mapping_options", dts_ann),
  md_lst("mapping_index", dts_ann)
)

chk <- 0

chk <- chk +   2 * (n_cmd == n_par)
chk <- chk +   4 * (n_cmd == n_idx)
chk <- chk +   8 * (n_par == n_idx)

chk <- chk +  16 * (n_cmd == 1)
chk <- chk +  32 * (n_par == 1)
chk <- chk +  64 * (n_idx == 1)

chk <- chk + 128 * (n_cmd > 1)
chk <- chk + 256 * (n_par > 1)
chk <- chk + 512 * (n_idx > 1)

err <- T
# n(command, parameters, index)
if(bitAnd(chk, 2 + 4 + 8) == 2 + 4 + 8) err <- F
# n(command), parameters, index
if(bitAnd(chk, 128 + 32 + 64) == 128 + 32 + 64) err <- F
# command, n(parameters), index
if(bitAnd(chk, 16 + 256 + 64) == 16 + 256 + 64) err <- F
# command, parameters, n(index)
if(bitAnd(chk, 16 + 32 + 512) == 16 + 32 + 512) err <- F
# *** TODO ***
# n(command, parameters), index
# command, n(parameters, index)
# n(command, index), parameters

if(err)  stop("incorrect mapping arguments")

n_dts <- max(n_cmd, n_par, n_idx)
DTSLST <- vector("list", n_dts)
output_paths <- rep("", n_dts)
DTSFILE <- rep("", n_dts)

for(i in 1:n_dts) {
  idx <- which(! colnames(dts_ann) %in% col_lst)
  idx <- c(idx, match(md_lst("mapping_command", dts_ann)[min(i, n_cmd)], colnames(dts_ann)))
  idx <- c(idx, match(md_lst("mapping_options", dts_ann)[min(i, n_par)], colnames(dts_ann)))
  idx <- c(idx, match(md_lst("mapping_index", dts_ann)[min(i, n_idx)], colnames(dts_ann)))
  DTSLST[[i]] <- dts_ann[, idx]
  colnames(DTSLST[[i]])[ncol(DTSLST[[i]]) - 2:0] <- c(
    "mapping_command", "mapping_options", "mapping_index"
  )
  DTSLST[[i]]$bam_file = paste0(output_list, ".bam")
  output_paths[i] <- paste0(
    OUTDIR, "/",
    unique(DTSLST[[i]][, "mapping_command"]), "/",
    unique(DTSLST[[i]][, "mapping_index"])
  )
  # dataset description
  DTSFILE[i]   <- with(
    DTSLST[[i]],
    paste0(
      ANNDIR, "/", cmd_args(1), ".",
      unique(mapping_command), "_", unique(mapping_index), ".", JOBID, ".txt"
    )
  )
}

# =============================================================================.
# Build mapping commands
# -----------------------------------------------------------------------------.
cmd_lst <- vector("list", length(DTSLST))
for(i in 1:n_dts) {
  cmd <- unique(DTSLST[[i]][, "mapping_command"])
  prm <- unique(DTSLST[[i]][, "mapping_options"])
  idx <- unique(DTSLST[[i]][, "mapping_index"])
  if(length(cmd) > 1) stop("heterogeneous mapping commands")
  if(length(prm) > 1) stop("heterogeneous mapping options")
  if(length(idx) > 1) stop("heterogeneous mapping indexes")

  idx <-  match(idx, MAPPING_INDEXES$identifier)
  idx <- MAPPING_INDEXES$index_path[idx]
  if(cmd == "bowtie") {
    exe <- paste0(
      "gunzip -c ", fastq_paths, " | bowtie -p ", THREADSNBR, " ",
      prm, "--sam ", idx, " /dev/stdin | samtools view -bSF4 - > ",
      output_paths[i], "/", output_list, ".bam"
    )
  }
  if(cmd == "bowtie2") {
    exe <- paste0(
      "gunzip -c ", fastq_paths, " | bowtie2 -p ", THREADSNBR, " ",
      prm, "-x ", idx, " -U /dev/stdin | samtools view -bSF4 - > ",
      output_paths[i], "/", output_list, ".bam"
    )
  }
  if(cmd == "bwa") {
    exe <- paste0(
      "bwa aln -t ", THREADSNBR, " ", prm, " ", idx, " <(gunzip -c '", fastq_paths, "')",
      " | bwa samse ", idx, " - <(gunzip -c '",  fastq_paths, "')",
      " | samtools view -bSF4 - > ", output_paths[i], "/", output_list, ".bam"
    )
  }
  if(cmd == "STAR") {
  }
  # cleanup
  exe <- stringr::str_trim(gsub(" +", " ", exe, perl = T))
  cmd_lst[[i]] <- exe
}

# START LOGFILE ################################################################

# =============================================================================.
# Create output directory if necessary and redirect outputs to log file
# -----------------------------------------------------------------------------.
LOGFILE <- log_file(paste0(ROOTDIR, "/", LOGDIR))
sink(LOGFILE, split = T)
sink(LOGFILE, type = "message")

# MAP READS ####################################################################

# =============================================================================.
setwd(ROOTDIR)
# -----------------------------------------------------------------------------.
CMDFILE <- file(CMDFILE, open = "a")
msg_header(
  paste(JOBID, "|", format(STARTTIME, "%d.%m.%Y %H:%M:%S")), file = CMDFILE
)
msg_command()
# -----------------------------------------------------------------------------.
cmd <- paste("mkdir -p", c(LOGDIR, ANNDIR, output_paths))
STATUS <- execute(cmd)
msg_command(cmd, "Make output directories", chrono = F, status = STATUS[1])
# =============================================================================.
# Map reads and save corresponding metadata
# -----------------------------------------------------------------------------.
for(i in 1:n_dts) {
  cmd <- cmd_lst[[i]]
  STATUS <- c(execute(cmd), STATUS)
  msg_command(cmd, "Map reads", status = STATUS[1])
  write.table(
    DTSLST[[i]], DTSFILE[i], quote = F, sep = "\t", row.names = F, col.names = T
  )
}
# =============================================================================.
