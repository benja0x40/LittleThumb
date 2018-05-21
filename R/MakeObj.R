# =============================================================================.
#' MakeObj
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{LoadObj},
#'   \link{SaveObj},
#'   \link{AvailableObj}
# -----------------------------------------------------------------------------.
#' @description
#' Generate an R object and save it as RData, or skip the generation and load
#' this object from previously saved RData if available.
#'
#' @details
#' TODO: documentation
#'
#' @param obj
#' an R object.
#'
#' @param ...
#' at least an expression defining the R object, which can be preceded by
#' optional parameters passed to the \link{LoadObj} and \link{SaveObj}
#' functions (see detail and example sections).
# -----------------------------------------------------------------------------.
#' @export
MakeObj <- function(obj, ...) {

  a <- match.call()
  n <- nargs()

  x <- a[[n + 1]]
  a[n + 1] <- NULL
  a[2] <- NULL
  a[1] <- call("list")
  a <- eval(a)

  if(is.null(a$name) & ! missing(obj)) {
    a$name <- deparse(substitute(obj))
  }
  if(n < 2 | is.null(a$name)) stop("insufficient arguments")

  cfg <- LittleThumb::lt_cfg() # LittleThumb options
  if(is.null(a$rebuild)) a$rebuild <- cfg$rebuild
  if(is.null(a$env)) a$env <- cfg$environment
  if(! is.environment(a$env)) a$env <- parent.frame()

  AO <- names(a) %in% formalArgs(AvailableObj)
  LO <- names(a) %in% formalArgs(LoadObj)
  SO <- names(a) %in% formalArgs(SaveObj)

  if(do.call(AvailableObj, a[AO]) & ! a$rebuild) {
    do.call(LoadObj, a[LO])
  } else {
    # env <- a$env
    eval(x, envir = a$env)
    do.call(SaveObj, c(list(obj = a$env[[a$name]]), a[SO]))
  }
}
