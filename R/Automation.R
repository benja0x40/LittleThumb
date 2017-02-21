# =============================================================================.
#
# -----------------------------------------------------------------------------.
installLittleThumb <- function() {
  # Find path of the executable script in package LittleThumb
  src <- paste0(exec_path(), "/littlethumb.R")
  if(! file.exists(src)) stop("try to rebuild or reinstall package LittleThumb")

  # Make link in ~/bin pointing to the executable script littlethumb.R
  bashrc  <- paste0(home_path(), "/.bashrc")
  usr_bin <- paste0(home_path(), "/bin")
  tgt <- paste0(usr_bin, "/littlethumb")
  if(! file.exists(tgt)) {
    cmd <- c(
      paste0("mkdir -p ", usr_bin),
      paste0("rm -f ", tgt),
      paste0("ln -s ", src, " ", tgt)
    )
    if(! all(execute(cmd))) stop("failed to make link to littlethumb")
  }
  txt <- readLines(bashrc)
  rex <- paste0("^export PATH\\=\\$PATH.*\\:(", usr_bin, "|\\$HOME/bin).*")
  if(! any(grepl(rex, txt, perl = T))) {
    txt <- c(
      str_line("=", begin = "# "),
      paste0("# Added by R package LittleThumb (", Sys.time(), ")"),
      str_line("-", begin = "# "),
      paste0("export PATH=$PATH:$HOME/bin"),
      str_line("-", begin = "# ")
    )
    bashrc <- file(bashrc, open = "a")
    writeLines(txt, con = bashrc)
    close(bashrc)
  }
  message("Installation successful")
  message("Please quit R and open a new terminal to refresh the PATH variable")
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
# startLittleThumb <- function() {
#
# }
