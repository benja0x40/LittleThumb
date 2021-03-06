% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MakePath.R
\name{MakePath}
\alias{MakePath}
\title{Construct a file path}
\usage{
MakePath(..., ext = NULL)
}
\arguments{
\item{...}{list of character vectors to be assembled into file paths.}

\item{ext}{file name extension(s) (default = "", none).}
}
\value{
\code{MakePath} returns a \code{character} vector.
}
\description{
\code{MakePath} (short alias \link{MkPath}) concatenates its arguments to
form corresponding filesystem paths, similarly to the base function
\link{file.path}.
}
\keyword{internal}
