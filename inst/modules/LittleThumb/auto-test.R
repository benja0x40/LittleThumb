# =============================================================================.
#
# -----------------------------------------------------------------------------.
DTSFILE  <- paste0(ANNDIR, "/", cmd_args(1), ".", JOBID, ".txt")
INPDIR   <- "."
OUTDIR   <- "."

# =============================================================================.
# Create output directory if necessary and redirect outputs to log file
# -----------------------------------------------------------------------------.
LOGFILE <- log_file(paste0(ROOTDIR, "/", LOGDIR))
sink(LOGFILE, split = T)
sink(LOGFILE, type = "message")

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

# =============================================================================.
# Create dataset file
# -----------------------------------------------------------------------------.
write.table(
  NULL, DTSFILE, quote = F, sep = "\t", row.names = F, col.names = T
)

