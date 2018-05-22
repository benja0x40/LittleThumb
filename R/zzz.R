# =============================================================================.
#
# -----------------------------------------------------------------------------.
.onAttach <- function(...) {

  # Insert LittleThumb entries in global options
  LittleThumb::ResetOptions()

}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
.onDetach <- function(...) {

  # Delete LittleThumb entries in global options
  cfg <- options()
  cfg <- cfg[grepl( "^LittleThumb\\.", names(cfg))]
  cfg[] <- vector("list", length(cfg))
  options(cfg)

}
