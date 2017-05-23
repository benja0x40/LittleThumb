# =============================================================================.
#
# -----------------------------------------------------------------------------.
DTSFILE <- paste0(ANNDIR, "/", cmd_args(1), ".", JOBID, ".txt")
INPDIR  <- "."
OUTDIR  <- "."
STATUS  <- T

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

x <- NULL
n <- length(cmd_args(as.string = F))
if(n == 2) x <- list()
if(n > 2) x <- as.list(cmd_args(3:n, as.string = F))
if(n > 1) {
  txt_out(x = "-")
  k <- suppressWarnings(! is.na(as.numeric(x)))
  x[k] <- as.numeric(x[k])
  r <- paste(x, collapse = ", ")
  txt_out("> ", cmd_args(2), "(", r, ")", sep = "")
  x <- do.call(what = cmd_args(2), args = x)
}

# =============================================================================.
# Create dataset file
# -----------------------------------------------------------------------------.
write.table(
  x, DTSFILE, quote = F, sep = "\t", row.names = F, col.names = T
)

