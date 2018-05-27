# =============================================================================.
#' Short alias of function MakeObj
# -----------------------------------------------------------------------------.
#' @description
#' \code{MkObj} is a short alias of function \link{MakeObj}.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MkObj <- function(...) { stop("not implemented") }

# =============================================================================.
#' Automatically make & save or load an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{LoadObj},
#'   \link{SaveObj},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @example examples/Basics.R
#'
#' @description
#' \code{MakeObj} automatically generates an R object using the provided
#' expression and saves the associated RDS file, or skips these two operations
#' and loads the previously saved RDS file when it is available, or even avoids
#' to do anything when the R object is already defined in the R environment.
#'
#' The automation performed by \code{MakeObj} can be controlled by optional
#' arguments and their corresponding global options which are accessible
#' via the \link{LittleThumb} function (see detail and example sections).
#'
#' \link{MkObj} is a short alias of function \code{MakeObj}.
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
#' @keywords visible
#' @export
MakeObj <- function(...) {

  x <- NULL
  a <- match.call()
  a <- ObjWithExpressionArgs(a, xpr = x)

  cfg <- LittleThumb() # Global options
  DefaultArgs(cfg, to = a)

  a <- as.list(a)

  if(! is.environment(a$envir)) a$envir <- parent.frame()

  a$rebuild <- LogicalArg(a$name, a$rebuild)

  protect <- c(a$name, objects(pos = a$envir))

  # Arguments forwarded to lower level functions
  AO <- names(a) %in% methods::formalArgs(AvailableObj)
  LO <- names(a) %in% methods::formalArgs(LoadObj)
  SO <- names(a) %in% methods::formalArgs(SaveObj)

  if(do.call(AvailableObj, a[AO]) & ! a$rebuild) {
    # Load the R object from existing RDS file
    do.call(LoadObj, a[LO])
  } else {
    # Make the R object by evaluating expression x
    eval(x, envir = a$envir)
    # TODO: Make sure that obj has been generated

    # Save RDS file associated to the R object
    do.call(SaveObj, c(list(obj = a$envir[[a$name]]), a[SO]))
  }

  if(a$cleanup) {
    lst <- setdiff(objects(pos = a$envir), protect)
    rm(list = lst, pos = a$envir)
  }
}
