# GENERIC ######################################################################

# =============================================================================.
# Interfaces
# -----------------------------------------------------------------------------.
make_path <- function (obj, ...) { UseMethod("make_path", obj) }
list_paths <- function (obj, ...) { UseMethod("list_paths", obj) }
create_paths <- function (obj, ...) { UseMethod("create_paths", obj) }
check_paths <- function (obj, ...) { UseMethod("check_paths", obj) }

lt_path <- function (obj, ...) { UseMethod("lt_path", obj) }
# lt_save <- function (obj, ...) { UseMethod("lt_save", obj) }
# lt_load <- function (con, ...) { UseMethod("lt_load", obj) }
# -----------------------------------------------------------------------------.
make_path.default <- function (obj, ...) {
  NextMethod("make_path", obj, ...)
}
list_paths.default <- function (obj, ...) {
  NextMethod("list_paths", obj, ...)
}
create_paths.default <- function (obj, ...) {
  NextMethod("create_paths", obj, ...)
}
check_paths.default <- function (obj, ...) {
  NextMethod("check_paths", obj, ...)
}
# -----------------------------------------------------------------------------.

# FUNCTIONS ####################################################################

# =============================================================================.
# clear_path
# -----------------------------------------------------------------------------.
clear_path <- function(x) {
  cmd <- paste0("cd ", dirname(x), "; rm -Rf ", basename(x))
  execute(cmd)
}
# =============================================================================.
#' lt_save
# -----------------------------------------------------------------------------.
#' @param obj object
#' @param file path or connexion
# -----------------------------------------------------------------------------.
lt_save <- function(obj, file, ...) {
  writeLines(obj2txt(obj, ...), file)
}
# =============================================================================.
#' lt_load
# -----------------------------------------------------------------------------.
#' @param file path or connexion
#' @return object
# -----------------------------------------------------------------------------.
lt_load <- function(file, ...) {
  # Read text
  txt <- readLines(file)
  # Extract class name
  cls <- read_obj_signature(txt)$class
  # Call constructor to retrieve parent classes
  cls <- class(do.call(what = cls, args = list()))
  # Set text class and parse text data accordingly
  class(txt) <- cls
  txt2obj(txt, ...)
}
# =============================================================================.
# Vectorized version of make_path
# -----------------------------------------------------------------------------.
make_paths <- function(obj, name, root = "", key = 1, value = 2) {

  vec_mkp <- function(name, obj, root, key, value) {
    make_path(obj, name = name, root = root, key = key, value = value)
  }
  sapply(name, FUN = vec_mkp, obj = obj, root = root, key = key, value = value)
}
# =============================================================================.
#' make_path
# -----------------------------------------------------------------------------.
#' @description
#' Path construction using depth search first.
#' Note: Does not support multiple occurence of the same path key in the path
#' structure.
#'
#' @param obj
#' data.frame containing two columns used to define paths as {key, value} pairs
#'
#' @param paths
#' list defining a file system tree structure
#'
#' @param name
#' key of the path to be build
#'
#' @param root
#' root location of the path to be build
#'
#' @param key
#' column name for path keys in the obj data.frame
#'
#' @param value
#' column name for path values in the obj data.frame
#'
#' @return
#' make_path returns a character value representing the requested file system
#' path.
# -----------------------------------------------------------------------------.
make_path.data.frame <- function(
  obj, paths, name, root = "", key = 1, value = 2
) {

  n <- length(paths)
  k <- gsub("^[0-9]+$", "", names(paths), perl = T)
  if(length(k) == 0) k <- rep("", n)

  x <- NULL
  if(name %in% k) {
    r <- register_value(obj, k = key, x = name, v = value)
    x <- make_path(root, r)
  }
  if(name %in% paths) {
    r <- register_value(obj, k = key, x = name, v = value)
    i <- match(name, paths)
    p <- k[i]
    if(p != "") p <- register_value(obj, k = key, x = p, v = value)
    x <- make_path(root, p, r)
  }
  i <- 1
  while(n > 1 & i <= n & is.null(x)) {
    p <- register_value(obj, k = key, x = k[i], v = value)
    x <- make_path(obj, paths[[i]], name, make_path(root, p))
    i <- i + 1
  }
  x
}
# =============================================================================.
#' make_path
# -----------------------------------------------------------------------------.
#' @param ... components of a file path
#' @param ext file name extension (default = "")
# -----------------------------------------------------------------------------.
#' @return make_path returns a \code{character} value
# -----------------------------------------------------------------------------.
make_path.character <- function(..., ext = "") {
  cleanSlashes <-function(x) {
    x <- gsub("[/]+", "/", x)
    x <- gsub("^/", "", x)
    x <- gsub("/$", "", x)
    if(x != "") x <- paste0(x, "/")
    x
  }
  path <- list(...)
  path <- path[! sapply(path, is.null)]
  if(length(path) > 0) {
    path <- lapply(path, as.character)
    root <- ""
    if(substr(path[1], 1, 1) == "/") root <- "/"
    n <- length(path)
    path[-n] <- sapply(path[-n], cleanSlashes)
    path <- paste0(root, paste(path, collapse = ""), ext)
    path <- gsub("[/]+", "/", path)
    # path <- gsub("[\\.]+([^\\.]+)$", ".\\1", path)
  } else {
    path <- NULL
  }
  path
}
