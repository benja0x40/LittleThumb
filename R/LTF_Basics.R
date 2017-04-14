# SYS INFO #####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
host_name <- function() {
  as.character(Sys.info()["nodename"])
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
home_path <- function() {
  as.character(Sys.getenv()["HOME"])
}

# PKG INFO #####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
exec_path <- function() {
  paste0(inst("LittleThumb"), "/exec")
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
modules_path <- function() {
  paste0(inst("LittleThumb"), "/modules")
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
config_path <- function() {
  paste0(inst("LittleThumb"), "/config")
}

# PKG MODULES ##################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
list_modules <- function(...) {
  dir(modules_path(), ...)
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
load_modules <- function() {
  lst <- paste0(list_modules(full.names = T), "/R")
  lst <- lst[file.exists(lst)]
  exe <- c()
  for(fpath in lst) {
    exe <- c(exe, dir(fpath, full.names = T, pattern = "\\.R$"))
  }
  sapply(exe, source, local = baseenv())
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
list_commands <- function(detailed = F) {
  tbl <- data.frame(stringsAsFactors = F)
  lst <- list_modules(full.names = T)
  for(elm in lst) {
    fpath <-  dir(elm, pattern = "\\.R$", full.names = T)
    if(length(fpath) > 0) {
      cmd   <-  gsub("\\.R$", "", basename(fpath), perl = T)
      tbl <- rbind(
        tbl, cbind(name = cmd, module = basename(elm), path = fpath),
        stringsAsFactors = F
      )
    }
  }
  if(! detailed) tbl <- tbl$name
  tbl
}

# SCRIPT INFO ##################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
cmd_name <- function() {
  cmd <- commandArgs()
  cmd <- basename(cmd[grepl("^--file=", cmd, perl = T)])
  cmd
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
cmd_args <- function(x = NULL, as.string = T) {
  r <- commandArgs(trailingOnly = T)
  if(length(r) == 0) {
    if(as.string) r <- ""
  } else {
    if(! is.null(x)) r <- r[x]
    if(as.string) r <- stringr::str_trim(base::paste(r, collapse = " "))
  }
  r
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
cmd_line <- function() {
  paste(cmd_name(), cmd_args())
}

# UTILITIES ####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
nbr.summary <- function(x, n = 5) {
  x <- table(x)
  k <- as.numeric(names(x)) > n
  x <- x[as.character(0:n)] + c(rep(0, n), sum(x[k]))
  x[is.na(x)] <- 0
  names(x)[6] <- paste(n, "+", sep = "")
  x
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
names2r <- function(x) {
  cat('c(\n', paste('  "', x, collapse = '",\n', sep = ""), '"\n)', sep ="")
}

# MESSAGES #####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
.ttmsg. <- function(a, b) {
  # txt_pad(txt_pad(x = " ", a, n = 23), "|", b, x = " ", n = 48) # identical
  paste(str_pad(a, 23), "|", str_pad(b, 22, side = "right"))
}

# =============================================================================.
# Examples:
# txt_pad("A", x = "-", "B", x = "-", "C", n = 15)
# c(
#   txt_pad(x = "/", "title", x = "/"),
#   txt_pad("=", "title", x = "="),
#   txt_pad(x = "-", "title", "-"),
#   txt_pad("name", x = ".", "description")
# )
# -----------------------------------------------------------------------------.
txt_pad <- function(..., sep = " ", n = NULL, indent = 0, split_lines = F) {

  s <- list(...)
  x <- names(s) == "x"
  s <- as.character(s)
  l <- nchar(s)

  # Resolve character expansions
  xc <- sum(l[x])
  cc <- sum(l[! x])
  sc <- nchar(sep) * (length(s) - 1)

  nx <- rep(0, length(s))

  if(sep == "\n") {

    if(is.null(n) & sum(x) > 0) n <- max(l[! x])
    nx[x] <- n

  } else {

    if(is.null(n)) n <- 80
    nx <- (l * ! x) + x * (n - (cc + sc)) %/% xc

    nt <- sum(nx) + sc
    if(xc > 1 & nt < n) {
      k <- which(x)[sum(x) - ((n - nt):1)]
      nx[k] <- nx[k] + 1
    }
  }

  # Make character expansions
  s[x] <- str_pad("", width = nx[x], pad = s[x])
  s <- paste(s, collapse = sep)

  # Make indentation
  if(indent > 0) {
    indent <- str_pad("", width = indent, pad = " ")
    s <- gsub("^", indent, s)
    s <- gsub("\n", paste0("\n", indent), s)
  }

  # Split lines
  if(split_lines) s <- unlist(str_split(s, "\n"))

  s
}

# =============================================================================.
# Try: txt_out("A", x = "-", "B", x = "-", "C", n = 15, sep = "\n", indent = 4)
# -----------------------------------------------------------------------------.
txt_out <- function(..., file = "", append = F) {
  txt <- txt_pad(...)
  cat(txt, "\n", sep = "", file = file, append = append)
}

# EXECUTION ####################################################################

# =============================================================================.
#' checklist
# -----------------------------------------------------------------------------.
#' @param chk logical vector
#' @param lst character vector
#' @param msg message
# -----------------------------------------------------------------------------.
#' @return NULL
# -----------------------------------------------------------------------------.
checklist <- function(chk, lst, msg = "", spc = "\n") {
  if(! all(chk)) {
    msg <- paste0(msg, spc, paste(lst[! chk], collapse = spc, sep = ""))
    stop(msg)
  }
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
confirm_execution <- function(q = T) {
  txt_out("Proceed [y/n]? ")
  x <- readLines(file("stdin"), n = 1)
  r <- T
  if(! grepl("^(y|yes)$", x, perl = T, ignore.case = T)) {
    cat("Cancelled\n")
    if(q) quit() else r <- F
  }
  r
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
execute <- function(x, ...) {
  x <- paste("echo", shQuote(x), "| bash")
  all(sapply(x, system, ...) == 0)
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
timepoint <- function(x) {
  x <- format(round(difftime(Sys.time(), x, units = "auto"), 2))
  x <- gsub("Time difference of ", "", x)
  x
}

# LOG/MESSAGES #################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
print_options <- function(opt, args, lbl) {
  k <- names(opt)
  v <- as.vector(unlist(opt))
  k <- str_pad(k, max(nchar(k)), side = "right", pad = " ")
  cat("Parameters:\n")
  cat(paste(paste0("  ", k, " = ", v), collpase = "\n"), sep = "")
  cat(paste0(lbl, ":\n"))
  cat(paste0("  ", paste(args, collapse = " "), "\n"))
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
log_file <- function(d = ".") {
  f <- paste0(
    cmd_args(1),
    ".", format(STARTTIME, "%d.%m.%Y"), "_", format(STARTTIME, "%Hh%Mm%S"), ".",
    JOBID
  )
  system(paste("mkdir -p", d))
  file(paste0(d, "/", f, ".txt"), open = "w")
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
msg_command <- function(cmd = NULL, comment = NULL, chrono = T, status = NULL) {
  if(! is.null(comment)) {
    writeLines(paste("#", comment), CMDFILE)
    if(VERBOSE) cat(paste0(comment, "...\n"))
  }
  if(! is.null(cmd)) writeLines(cmd, CMDFILE)
  if(! is.null(status)) {
    writeLines(paste0("# (completion = ", format(status), ")"), CMDFILE)
  }
  if(chrono) {
    writeLines(paste0("# (time = ", timepoint(STARTTIME), ")"), CMDFILE)
  }
  writeLines("", CMDFILE)
  flush(CMDFILE)
}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
msg_header <- function(tag, ...) {
  txt_out("#", x = "=", ...)
  txt_out("# [ ", tag, " ] ", cmd_args(1), sep = "", ...)
  txt_out("#", x = "-", ...)
  txt_out("# COMMAND = ", cmd_line(), sep = "", ...)
  txt_out("# WORKDIR = ", LAUNCHDIR, sep = "", ...)
  txt_out("#", x = "-", ...)
}
