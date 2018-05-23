# =============================================================================.
#
# -----------------------------------------------------------------------------.
.onAttach <- function(...) {

  # Initialize global options of the LittleThumb package
  LittleThumb::ResetOptions()

}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
.onDetach <- function(...) {

  # Remove global options of the LittleThumb package from the R environment
  LittleThumb::RemoveOptions()

}
