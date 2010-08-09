\name{extension}

\alias{ext}
\alias{ext<-}

\title{Filename extensions}

\description{
Get or change a filename extension
}

\usage{
ext(filename, value=NULL)
ext(filename) <- value
}

\arguments{
\item{filename}{A filename, with or without the path}
\item{value}{A file extension including the dot, e.g., ".txt" }
}

\value{
A file extension, filename or path.  

If \code{ext(filename)} is used without a \code{value} argument, it returns the file extension; otherwise it returns the filename (with new extions set to \code{value}
}

\author{Robert J. Hijmans }


\examples{
fn <- "c:/temp folder/filename.exten sion"
ext(fn)
ext(fn) <- ".txt"
ext(fn)
ext(fn, '.document')
}

\keyword{file}

