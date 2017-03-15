# CONFIG #######################################################################

# =============================================================================.
# Option to switch between testing and normal modes
# -----------------------------------------------------------------------------.
.pkg_testing.  <- function() {

  testing <- options()$LittleThumb.testing
  if(is.null(testing)) {
    stop("missing internal package option LittleThumb.testing")
  }

  testing
}

# =============================================================================.
# Path to LittleThumb's user dir
# -----------------------------------------------------------------------------.
.usr_dir.  <- function() {
  ifelse(.pkg_testing.(), "~/.LittleThumbTest", "~/.LittleThumb")
}

# =============================================================================.
# Name of LittleThumb's configuration file
# -----------------------------------------------------------------------------.
.pkg_cfg.  <- function() {
  ifelse(.pkg_testing.(), "TestConfig", "DefaultConfig")
}

# =============================================================================.
# Path to LittleThumb's configuration file
# -----------------------------------------------------------------------------.
.pkg_config_file. <- function() {
  if(.pkg_testing.()) {
    if(basename(getwd()) == "LittleThumb") {
      flp <- "inst/config/"
    }
    if(basename(getwd()) == "testthat") {
      flp <- "../../inst/config/"
    }
    flp <- paste0(normalizePath(flp), "/", .pkg_cfg.())
  } else {
    flp <- paste0(inst("LittleThumb"), "/config/", .pkg_cfg.())
  }
  flp
}

# =============================================================================.
# Generator of LittleThumb's default configuration
# -----------------------------------------------------------------------------.
.make_pkg_config. <- function(save = T) {

  if(! .pkg_testing.()) stop("forbidden, use option(LittleThumb.testing = T)")

  # Default path register
  path_register <- matrix(
    c(
      # label       | value                            | is_dir | level
      "USRDIR",      .usr_dir.(),                       "T",     "user",
      "LTEFILE",      "LittleThumbEnvironment.RData",   "F",     "user",
      "MAINDIR",     "_LittleThumb_",                   "T",     "workspace",
      "CFGDIR",      "config",                          "T",     "workspace",
      "DTSDIR",      "datasets",                        "T",     "workspace",
      "JBSDIR",      "jobs",                            "T",     "workspace",
      "CMDDIR",      "commands",                        "T",     "workspace",
      "HISTFILE",    "history.txt",                     "F",     "workspace"
    ),
    nrow = 8, byrow = T
  )
  TD <- LT_TabularData(data.frame(path_register, stringsAsFactors = F))
  colnames(TD) <- c("label", "value", "is_dir", "level")
  TD$is_dir <- as.logical(TD$is_dir)

  # Default path structure
  path_structure = list(
    USRDIR = list("LTEFILE"),
    MAINDIR = list("CFGDIR", "DTSDIR", "JBSDIR", "CMDDIR", "HISTFILE")
  )
  names(path_structure[[1]]) <- 1:length(path_structure[[1]])
  names(path_structure[[2]]) <- 1:length(path_structure[[2]])

  # Default options
  MD <- LT_MetaData(
    retry   = list(nmax = 10, delay = 60),
    storage = list(datasets = "text", jobs = "text", history = "text"),
    paths = path_structure
  )

  cfg <- LT_Config(name = .pkg_cfg.(), MD = MD, TD = TD)

  flp <- .pkg_config_file.() # Always use path to package source
  if(save) lt_save(cfg, flp, pretty = T)

  flp
}

# INSTALL ######################################################################

# =============================================================================.
#' installLittleThumb
# -----------------------------------------------------------------------------.
#' @description
#' Must be run after standard installation of the R package.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
installLittleThumb <- function() {

  # Find path of the executable script in package LittleThumb
  src <- paste0(exec_path(), "/littlethumb.R")
  if(! file.exists(src)) stop("try to rebuild or reinstall package LittleThumb")

  # Make link in ~/bin pointing to the executable script littlethumb.R
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

  # Add path to ~/bin/littlethumb
  bashrc  <- paste0(home_path(), "/.bashrc")
  if(! file.exists(bashrc)) {
    cmd <- paste("touch", bashrc)
    if(! all(execute(cmd))) stop("failed to create ~/.bashrc file")
  }

  txt <- readLines(bashrc)
  rex <- paste0("^export PATH\\=\\$PATH.*\\:(", usr_bin, "|\\$HOME/bin).*")
  if(! any(grepl(rex, txt, perl = T))) {
    txt <- c(
      txt_pad("#", x = "="),
      paste0("# Added by R package LittleThumb (", Sys.time(), ")"),
      txt_pad("#", x = "-"),
      paste0("export PATH=$PATH:$HOME/bin"),
      txt_pad("#", x = "-")
    )
    bashrc <- file(bashrc, open = "a")
    writeLines(txt, con = bashrc)
    close(bashrc)
  }

  message("Installation successful")
  message("Please quit R and open a new terminal to refresh the PATH variable")
}
