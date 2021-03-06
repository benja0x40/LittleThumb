% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AvailableObj.R
\name{AvailableObj}
\alias{AvailableObj}
\title{Check if the RDS file associated to an R object is available}
\usage{
AvailableObj(
  obj,
  path = NULL,
  name = NULL,
  parent = NULL,
  parent.name = NULL,
  relative = NULL,
  embedded = NULL,
  origin = parent.frame()
)
}
\arguments{
\item{obj}{an R object.}

\item{path}{folder location for RDS files. LittleThumb interprets this path either as
a sub-directory of the \code{rootpath} location, or as an independent
location (see \code{relative}). By default \code{path} is ignored
(\code{path = NA}).}

\item{name}{\strong{RESERVED FOR INTERNAL USE}: optional name of the R object.
When omitted this name is automatically provided by the symbol being passed
as first argument using \link{substitute} and \link{deparse}.}

\item{parent}{\link{environment} where the R object should be located.
By default \code{parent} is the \link{parent.frame} of the function
being called.}

\item{parent.name}{optional name of the parent environment.}

\item{relative}{logical value controlling if the \code{path} option is interpreted as a
relative location, meaning as a sub-directory of the \code{rootpath}
location (default = \code{TRUE}, yes), or as an independent one.}

\item{embedded}{logical value controlling whether child objects should have their associated
RDS files organized into sub-directories corresponding to parent objects
(default = \code{TRUE}, yes).}

\item{origin}{\strong{RESERVED FOR INTERNAL USE}: parent.frame of the initial function
call.}
}
\value{
\code{TRUE} when the RDS file exists, \code{FALSE} otherwise.
}
\description{
Check if the RDS file associated to an R object is available
}
\details{
When unspecified, the value of the following argument(s) are determined
by the corresponding automation option(s) (see \link{LittleThumb}):

\code{relative} and \code{embedded}
}
\examples{
# Create a simple R object named 'a'
a <- 1:10

# Save object a in the current working directory
SaveObj(a)

# Simulate restarting the R environment
rm(a)

# Check if object a is available in the R environment (FALSE)
exists("a")

# Check if an RDS file is available for object a (TRUE)
AvailableObj(a)

# Load object a from the current working directory
LoadObj(a)

print(a)

# Delete the RDS file associated to object a and remove the object itself
DeleteObj(a)

# Now both lines below should return FALSE
AvailableObj(a)
exists("a")

# A verbose way to automatically make/save/load the object 'a' (don't use this)
if(AvailableObj(a)) {
  LoadObj(a)
} else {
  a <- 1:10
  SaveObj(a)
}

# Automatically make/save/load the object 'a'
MakeObj(a, { a <- 1:10 })

# Cleanup ======================================================================
DeleteObj(a)
unlink("AutoSaved", recursive = TRUE)
}
\seealso{
\link{MakeObj},
  \link{LittleThumb}
}
\keyword{internal}
