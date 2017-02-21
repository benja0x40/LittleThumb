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
MAPPING_INDEXES <- paste0(config_path(), "/MAPPING_INDEXES.txt")

if(! file.exists(MAPPING_INDEXES)) stop("package is broken")
MAPPING_INDEXES <- read.delim(MAPPING_INDEXES, stringsAsFactors = F)
MAPPING_INDEXES <- availableMappingIndexes(MAPPING_INDEXES)
MAPPING_INDEXES <- filter(MAPPING_INDEXES, hostname == host_name())
# =============================================================================.
# Internal configuration
# -----------------------------------------------------------------------------.
STARTTIME <- Sys.time()
TODAY     <- format(STARTTIME, "%d.%m.%Y")
JOBID     <- job_identifier()
LAUNCHDIR <- getwd()
CMDFILE   <- "commands.sh"
AUTODIR   <- ".AUTOMATION."
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
# -----------------------------------------------------------------------------.
msg_line("=")
print(cmd_line()) # help to manage command syntaxes
msg_line()
print(ls()) # help to make sure modules start within a clean environment
msg_line()
# -----------------------------------------------------------------------------.
rex <- "^import-geo +"
if(grepl(rex, cmd_args(), perl = T)) {
  JOBARGS <- gsub(rex, "", cmd_args())
  rm(rex)
  source(paste0(modules_path(), "/ImportDataFromGEO.R"))
}
# -----------------------------------------------------------------------------.
rex <- "^import-basespace +"
if(grepl(rex, cmd_args(), perl = T)) {
  JOBARGS <- gsub(rex, "", cmd_args())
  rm(rex)
  source(paste0(modules_path(), "/ImportDataFromBaseSpace.R"))
}
# -----------------------------------------------------------------------------.
rex <- "^map-reads +"
if(grepl(rex, cmd_args(), perl = T)) {
  JOBARGS <- gsub(rex, "", cmd_args())
  rm(rex)
  source(paste0(modules_path(), "/ReadsMapping.R"))
}
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
