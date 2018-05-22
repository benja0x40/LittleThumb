# =============================================================================.
#
# -----------------------------------------------------------------------------.
.onAttach <- function(...) {

  # Path generation
  options(LittleThumb.path = "")
  options(LittleThumb.root = T)
  options(LittleThumb.extension = ".rds")

  # Behavior
  options(LittleThumb.makedir  = T)
  options(LittleThumb.rebuild  = F)
  options(LittleThumb.overload = F)
  options(LittleThumb.messages = T)

  # Evaluation
  options(LittleThumb.environment = NA)

}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.onDetach <- function(...) {

  # Path generation
  options(LittleThumb.path = NULL)
  options(LittleThumb.root = NULL)
  options(LittleThumb.extension = NULL)

  # Behavior
  options(LittleThumb.makedir  = NULL)
  options(LittleThumb.rebuild  = NULL)
  options(LittleThumb.overload = NULL)
  options(LittleThumb.messages = NULL)

  # Evaluation
  options(LittleThumb.environment = NULL)

}
