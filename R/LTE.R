# INCLUDES #####################################################################

# Generic and regular functions
#' @include LTF_Basics.R
#' @include LTF_R_Objects.R
#' @include LTF_Registration.R
#' @include LTF_TextRepresentation.R
#' @include LTF_FileSystem.R

# Class definitions & implementations
#' @include LTC_TabularData.R
#' @include LTC_MetaData.R
#' @include LTC_Data.R
#' @include LTC_Config.R

# LTE ##########################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
.lte_name. <- function() {
  ifelse(.pkg_testing.(), "LTE", ".LTE.")
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.lte_env. <- function() {
  get(.lte_name.(), pos = globalenv())
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.lte_is_loaded. <- function(silent = T, error = F) {
  chk <- F
  if(exists(.lte_name.(), where = globalenv())) {
    if(! silent) message("LittleThumb is active")
    chk <- T
  } else {
    if(error) stop("LittleThumb is not active")
  }
  chk
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.lte_path. <- function(cfg) {
  make_path(cfg, "LTEFILE")
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.lte_save. <- function() {
  chk <- .lte_is_loaded.(error = T)
  LTE <- .lte_env.()
  saveRDS(LTE, .lte_path.(LTE$config))
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
resetLittleThumb <- function(ask = T) {

  msg <- ifelse(options()$LittleThumb.testing, " : TESTING", "")
  msg <- paste0("Reset LittleThumb", msg)
  if(ask) txt_out(x = "=", msg, x = "-", sep ="\n")

  # Load package config
  cfg <- lt_load(.pkg_config_file.())
  flp <- .lte_path.(cfg)
  usr <- dirname(flp)

  rst <- del <- xxx <- T
  if(ask) {
    message("Irreversible deletion of active environment")
    rst <- confirm_execution(q = F)
  }
  if(ask) {
    message("Irreversible deletion of saved environment:")
    message(flp)
    del <- confirm_execution(q = F)
  }
  if(ask) {
    message("Irreversible deletion of storage folder:")
    message(usr)
    xxx <- confirm_execution(q = F)
  }
  if(rst) {
    if(.lte_is_loaded.()) {
      suppressWarnings(rm(list = .lte_name.(), pos = globalenv()))
      message("LittleThumb cleared")
    }
  }
  if(del) {
    if(file.exists(flp)) {
      chk <- suppressWarnings(file.remove(flp))
      message("LittleThumb file deleted")
    }
  }
  if(xxx) {
    if(file.exists(usr)) {
      clear_path(usr)
      message("Storage folder deleted")
    }
  }
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
openLittleThumb <- function() {

  if(! .lte_is_loaded.(silent = F)) {

    # Load package config
    cfg <- lt_load(.pkg_config_file.())

    # Load user environement if available
    flp <- .lte_path.(cfg)
    if(file.exists(flp)) {
      assign(.lte_name.(), readRDS(flp), pos = globalenv())
    } else {
      # Create per user storage space for automation
      cmd <- paste("mkdir -p", make_path(cfg, "USRDIR"))
      execute(cmd)

      # Create environment
      chk <- assign(
        .lte_name.(), new.env(parent = globalenv()), pos = globalenv()
      )
      LTE <- .lte_env.()
      LTE$start_time       <- Sys.time()
      LTE$config           <- cfg   # LT_Config
      LTE$workspaces       <- LT_TabularData()
      LTE$graph            <- c()   # graph of dependencies
      .lte_save.()
    }
  }
}
