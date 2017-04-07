#!/usr/bin/env Rscript
# LIBRARIES ####################################################################

# CRAN ------------------------------------------------------------------------.
# Loaded as package dependencies by LittleThumb

# Bioconductor ----------------------------------------------------------------.
# Loaded as package dependencies by LittleThumb

# Github ----------------------------------------------------------------------.
suppressPackageStartupMessages(library(LittleThumb))

# GLOBAL OPTIONS ###############################################################

# Force file connexions to remain opened up to 10 days
options(timeout = 10 * 24 * 60 * 60)

# Prevent download.file errors when executing getGEO()
options('download.file.method.GEOquery' = 'libcurl')
# Alternatively:
# options('download.file.method.GEOquery' = 'wget')

# COMMON CONFIG ################################################################

# =============================================================================.
# External configuration
# -----------------------------------------------------------------------------.
MAPPING_INDEXES <- make_path(config_path(), "MAPPING_INDEXES.txt")

if(! file.exists(MAPPING_INDEXES)) stop("package is broken")
MAPPING_INDEXES <- read.delim(MAPPING_INDEXES, stringsAsFactors = F)
MAPPING_INDEXES <- availableMappingIndexes(MAPPING_INDEXES)
MAPPING_INDEXES <- filter(MAPPING_INDEXES, hostname == host_name())
# =============================================================================.
# Internal configuration
# -----------------------------------------------------------------------------.
STARTTIME <- Sys.time()
TODAY     <- format(STARTTIME, "%d.%m.%Y")
JOBID     <- make_id()
LAUNCHDIR <- getwd()
CMDFILE   <- "commands.sh"
ANNDIR    <- "_METADATA_"
JOBFILE   <- paste0(ANNDIR, "/JOBS.txt")
LOGDIR    <- "_LOGFILES_"
MAXTRIES <- 10
TRYDELAY <- 60
# =============================================================================.
# Default user options
# -----------------------------------------------------------------------------.
ROOTDIR    <- "."
ASK2RUN    <- TRUE
VERBOSE    <- TRUE
THREADSNBR <- 4

# RESOLVE TASK #################################################################

# =============================================================================.
# Developer informations (to be hidden)
# help to manage command syntaxes and to make sure modules start within a clean
# environment
# -----------------------------------------------------------------------------.
txt_out(x = "=")
txt_out(cmd_line())
txt_out(x = "-")
txt_out(paste(ls(), collapse = "\n"))
txt_out(x = "-")
# -----------------------------------------------------------------------------.
JOBARGS <- cmd_args()
# -----------------------------------------------------------------------------.
for(cmd in list_commands()) {
  rex <- paste0("^", cmd, " ")
  if(grepl(rex, JOBARGS, perl = T)) {
    JOBARGS <- job_args(rex, JOBARGS)
    rm(rex)
    source(
      list_commands(detailed = T)$path[match(cmd, list_commands())],
      echo = F, verbose = F
    )
  }
}
# -----------------------------------------------------------------------------.
# rex <- "^(littlethumb) +map-reads +(.*)"
# if(grepl(rex, JOBARGS, perl = T)) {
#   JOBARGS <- job_args(rex, JOBARGS)
#   rm(rex)
#   source(paste0(modules_path(), "/ReadsMapping.R"))
# }
# -----------------------------------------------------------------------------.

# EXIT #########################################################################

STATUS <- all(STATUS)

# =============================================================================.
# Save job informations
# -----------------------------------------------------------------------------.
txt <- c(
  jobid       = JOBID,
  date        = format(STARTTIME, "%d.%m.%Y"),
  time        = format(STARTTIME, "%H:%M:%S"),
  duration    = timepoint(STARTTIME),
  completion  = STATUS,
  dataset     = basename(DTSFILE),
  action      = cmd_name(),
  parameters  = cmd_args(),
  # launch.path = LAUNCHDIR,
  # root.path   = ROOTDIR,
  input.dir   = INPDIR,
  output.dir  = OUTDIR
)
if(! file.exists(JOBFILE)) {
  cat(paste(names(txt), collapse = "\t"), "\n", file = JOBFILE, sep = "")
}
cat(paste(txt, collapse = "\t"), "\n", file = JOBFILE, sep = "", append = T)
# =============================================================================.
# Close file connexions
# -----------------------------------------------------------------------------.
sink(NULL)
sink(NULL, type = "message")
close(LOGFILE)
close(CMDFILE)
# =============================================================================.
# Quit
# -----------------------------------------------------------------------------.
q(save = "no", status = ! STATUS)
