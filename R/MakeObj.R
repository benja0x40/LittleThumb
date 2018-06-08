# =============================================================================.
#' Short alias of function MakeObj
# -----------------------------------------------------------------------------.
#' @description
#' \code{MkObj} can be used as a short alias of function \link{MakeObj}.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
MkObj <- function(...) {
  a <- match.call()
  a[1] <- call("MakeObj")
  eval(a, envir = parent.frame())
}

# =============================================================================.
#' Automatically make & save or load an R object
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LittleThumb},
#'   \link{DeleteObj}
# -----------------------------------------------------------------------------.
#' @example exec/examples/Basics.R
#'
#' @description
#' \code{MakeObj} automatically generates an R object using the provided
#' expression and saves the associated RDS file, or skips these two operations
#' and loads the previously saved RDS file when it is available, or even avoids
#' to do anything when the object is already defined in its parent environment.
#'
#' The automation performed by \code{MakeObj} can be controlled by optional
#' arguments and their corresponding global options which are accessible
#' via the \link{LittleThumb} function (see the details section).
#'
#' @note
#' \link{MkObj} can be used as a short alias of function \code{MakeObj}.
#'
#' @details
#' The \code{MakeObj} function requires at least two arguments: an R symbol
#' in the first position and an R expression in the last position,
#' both being provided as unnamed arguments.
#'
#' When evaluated, the provided expression must define the value
#' of the R object to be automatically saved or loaded.
#' For instance, the following line would automatically make/save/load
#' an integer vector named \code{x} and containing values from 1 to 10:
#'
#' \code{MakeObj(x, { x <- 1:10 })}
#'
#' In addition, the following named arguments can be used in between
#' the symbol and expression arguments (see \link{LittleThumb}):
#'
#' \code{rebuild}, \code{overload}, \code{path} and \code{parent}
#'
#' @param ...
#' at least an R symbol and an expression defining the corresponding R object.
#' The symbol must be provided in first position and the expression in last
#' position, both as unnamed arguments.
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

  if(! (is.list(a$parent) | is.environment(a$parent))) {
    a$parent <- parent.frame()
  }

  a$rebuild <- LogicalArg(a$name, a$rebuild)

  protect <- c(a$name, objects(pos = a$parent))

  # Arguments forwarded to lower level functions
  AO <- names(a) %in% methods::formalArgs(AvailableObj)
  LO <- names(a) %in% methods::formalArgs(LoadObj)
  SO <- names(a) %in% methods::formalArgs(SaveObj)

  if(do.call(AvailableObj, a[AO]) & ! a$rebuild) {
    # Load the R object from existing RDS file
    f <- do.call(LoadObj, a[LO])
  } else {
    # Make the R object by evaluating expression x
    eval(x, envir = a$parent)
    # TODO: Make sure that obj has been generated

    # Save RDS file associated to the R object
    f <- do.call(SaveObj, c(list(obj = a$parent[[a$name]]), a[SO]))
  }

  if(a$cleanup) {
    lst <- setdiff(objects(pos = a$parent), protect)
    rm(list = lst, pos = a$parent)
  }

  invisible(f)
}
