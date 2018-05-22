# =============================================================================.
#' Automatically make & save or load an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LoadObj},
#'   \link{SaveObj},
#'   \link{LittleThumb}
# -----------------------------------------------------------------------------.
#' @example examples/Basics.R
#'
#' @description
#' \code{MakeObj} automatically generates an R object using the provided
#' expression and saves the associated RDS file, or skips these two operations
#' and loads the previously saved RDS file when it is available, or even avoids
#' to do anything when the R object is already defined in the R environment.
#'
#' The automation performed by MakeObj can be controlled by optional arguments
#' and their corresponding global options which are accessible via the
#' \link{LittleThumb} function (see detail and example sections).
#'
#' @details
#' The \code{MakeObj} function requires at least two arguments: an R symbol
#' in the first position (i.e. \code{obj} argument) and an R expression
#' in the last position.
#' More precisely, the last argument of \code{MakeObj} must be the expression
#' generating the R object to be automatically saved or loaded and associated
#' to the symbol \code{obj}.
#'
#' For instance, the following line would automatically make/save/load
#' an integer vector named \code{x} and containing values from 1 to 10:
#'
#' \code{MakeObj(x, { x <- 1:10 })}
#'
#' The expression provided as last argument can be preceded by optional the
#' arguments \code{rebuild} and \code{overlaod} to control the behavior of
#' \code{MakeObj}, as well as \code{path} and \code{env} to control the
#' location of generated RDS files and R objects (see \link{LittleThumb}).
#'
#' @param obj
#' an R symbol.
#'
#' @param ...
#' at least an expression defining the R object, which can be preceded by
#' optional arguments (see detail and example sections).
# -----------------------------------------------------------------------------.
#' @export
MakeObj <- function(obj, ...) {

  a <- match.call()
  n <- nargs()

  # Last argument must be an expression generating the R object
  x <- a[[n + 1]]
  a[n + 1] <- NULL
  a[2] <- NULL
  a[1] <- call("list")
  a <- as.environment(eval(a))

  if(is.null(a$name) & ! missing(obj)) {
    a$name <- deparse(substitute(obj))
  }
  if(n < 2 | is.null(a$name)) stop("insufficient arguments")

  cfg <- LittleThumb() # Global options
  DefaultArgs(AvailableObj, cfg, ignore = "path", env = a)
  a <- as.list(a)

  if(! is.environment(a$envir)) a$envir <- parent.frame()

  a$rebuild <- LogicalArg(a$name, a$rebuild)

  AO <- names(a) %in% formalArgs(AvailableObj) # Arguments for AvailableObj
  LO <- names(a) %in% formalArgs(LoadObj)      # Arguments for LoadObj
  SO <- names(a) %in% formalArgs(SaveObj)      # Arguments for SaveObj

  if(do.call(AvailableObj, a[AO]) & ! a$rebuild) {
    # Load the R object from existing RDS file
    do.call(LoadObj, a[LO])
  } else {
    # Make the R object by evaluating expression x
    eval(x, envir = a$envir)
    # Save RDS file associated to the R object
    do.call(SaveObj, c(list(obj = a$envir[[a$name]]), a[SO]))
  }
}
