# GENERIC ######################################################################

# =============================================================================.
# Interfaces
# -----------------------------------------------------------------------------.
# lt_name <- function (obj, ...) { UseMethod("lt_name", obj) }
# `lt_name<-` <- function (obj, ...) { UseMethod("lt_name<-", obj) }
# -----------------------------------------------------------------------------.
is_key <- function (obj, ...) { UseMethod("is_key", obj) }
is_registered <- function (obj, ...) { UseMethod("is_registered", obj) }
# -----------------------------------------------------------------------------.
register_value <- function (obj, ...) { UseMethod("register_value", obj) }
`register_value<-` <- function (obj, ...) { UseMethod("register_value<-", obj) }
# -----------------------------------------------------------------------------.
register_filter <- function (obj, ...) { UseMethod("register_filter", obj) }
register_split <- function (obj, ...) { UseMethod("register_split", obj) }
register_merge <- function (...) {
  obj <- list(...)[[1]]
  UseMethod("register_merge", obj)
}
# -----------------------------------------------------------------------------.
