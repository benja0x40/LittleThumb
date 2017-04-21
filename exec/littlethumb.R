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

# LITTLETHUMB | START ##########################################################

openLittleThumb()

# COMMON CONFIG ################################################################

# =============================================================================.
# External configuration
# -----------------------------------------------------------------------------.
# MAPPING_INDEXES <- make_path(config_path(), "MAPPING_INDEXES.txt")
# if(! file.exists(MAPPING_INDEXES)) stop("package is broken")
# MAPPING_INDEXES <- read.delim(MAPPING_INDEXES, stringsAsFactors = F)
# MAPPING_INDEXES <- availableMappingIndexes(MAPPING_INDEXES)
# MAPPING_INDEXES <- filter(MAPPING_INDEXES, hostname == host_name())

MAPPING_INDEXES <- availableMappingIndexes()

# =============================================================================.
# Internal configuration
# -----------------------------------------------------------------------------.
STARTTIME <- Sys.time()
TODAY     <- format(STARTTIME, "%d.%m.%Y")
JOBID     <- make_id()
LAUNCHDIR <- getwd()
ANNDIR    <- "_ANNOTATIONS_" # make_path(lt_env()$config, "DTSDIR")
CMDFILE   <- make_path(lt_env()$config, "CMDFILE")  # "commands.sh"
JOBFILE   <- make_path(lt_env()$config, "JOBSFILE") # paste0(ANNDIR, "/JOBS.txt")
LOGDIR    <- make_path(lt_env()$config, "LOGSDIR")  # "_LOGFILES_"
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
for(cmd in list_commands()) {
  rex <- paste0("^", cmd, "( |$)")
  if(grepl(rex, cmd_args(1), perl = T)) {
    rm(rex)
    source(
      list_commands(detailed = T)$path[match(cmd, list_commands())],
      echo = F, verbose = F
    )
  }
}
# -----------------------------------------------------------------------------.

# EXIT #########################################################################

STATUS <- all(STATUS)

# =============================================================================.
# Save job informations
# -----------------------------------------------------------------------------.
for(dts in DTSFILE) {
  txt <- c(
    jobid       = JOBID,
    date        = format(STARTTIME, "%d.%m.%Y"),
    time        = format(STARTTIME, "%H:%M:%S"),
    duration    = timepoint(STARTTIME),
    completion  = STATUS,
    dataset     = basename(dts),
    action      = cmd_args(1),
    parameters  = cmd_args(-1),
    # launch.path = LAUNCHDIR,
    # root.path   = ROOTDIR,
    input.dir   = INPDIR,
    output.dir  = OUTDIR
  )
  if(! file.exists(JOBFILE)) {
    cat(paste(names(txt), collapse = "\t"), "\n", file = JOBFILE, sep = "")
  }
  cat(paste(txt, collapse = "\t"), "\n", file = JOBFILE, sep = "", append = T)
}
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
