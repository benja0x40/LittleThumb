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
.lte_path. <- function(cfg) {
  make_path(cfg, "LTEFILE")
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.lte_is_loaded. <- function(silent = T, error = F) {
  chk <- exists(.lte_name.(), where = globalenv())
  msg <- ifelse(chk, "LittleThumb is active", "LittleThumb is not active")
  if(error & ! chk) stop(msg)
  if(! silent) message(msg)
  chk
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
lt_env <- function(auto_start = T, silent = T, error = ! auto_start) {
  if(auto_start & ! .lte_is_loaded.(silent, error)) openLittleThumb()
  get(.lte_name.(), pos = globalenv())
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
lt_cfg <- function() {
  lt_env()$config
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.lte_save. <- function() {
  LTE <- lt_env(auto_start = F)

  opn <- LTE$workspaces$is_opened
  if(length(opn) > 0) LTE$workspaces$is_opened <- F

  saveRDS(LTE, .lte_path.(LTE$config))

  LTE$workspaces$is_opened <- opn
}
# =============================================================================.
#' openLittleThumb
# -----------------------------------------------------------------------------.
#' @description
#' Load or create the LittleThumb environement (LTE), which contains
#' configuration options as well as the workspace, dataset and job registers.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
openLittleThumb <- function() {

  if(! .lte_is_loaded.()) {

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
      LTE <- lt_env()
      LTE$start_time       <- Sys.time()
      LTE$config           <- cfg   # LT_Config
      LTE$workspaces       <- LT_TabularData()
      LTE$datasets         <- LT_TabularData()
      LTE$jobs             <- LT_TabularData()
      LTE$graph            <- empty_graph()   # graph of dependencies
      .lte_save.()
    }
  }
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
closeLittleThumb <- function(ask = T) {
  if(.lte_is_loaded.()) {


    LTE <- lt_env()
    lst <- list_workspaces(detailed = F, is_opened == TRUE)

    # 1. Confirm execution
    if(ask) {
      message("Close the following workspaces")
      txt_out(lst, sep = "\n", indent = 2)
      ans <- confirm_execution(q = F)
    }

    # 2. Close workspaces
    # TODO: handle this in close_workspace
    lst <- lst[lst %in% ls(envir = globalenv())]
    if(length(lst) > 0) close_workspace(lst)

    LTE$workspaces$is_opened <- F

    # 3. Save LTE
    .lte_save.()

    # 4. Remove LTE from R session
    suppressWarnings(rm(list = .lte_name.(), pos = globalenv()))
  }
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
    message("Irreversible deletion of saved environment")
    message(flp)
    del <- confirm_execution(q = F)
  }
  if(ask) {
    message("Irreversible deletion of storage folder")
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
