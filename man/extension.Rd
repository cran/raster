\name{extension}

\alias{ext}
\alias{ext<-}

\title{Filename extensions}

\description{
Get or change a filename extension
}

\usage{
ext(filename, value=NULL, maxchar=10)
ext(filename) <- value
}

\arguments{
\item{filename}{A filename, with or without the path}
\item{value}{A file extension with or without a dot, e.g., ".txt" or "txt"}
\item{maxchar}{Maximum number of characters after the last dot in the filename, for that string to be considered a filename extension }
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
fn <- ext(fn, '.document')
ext(fn)
ext(fn, maxchar=4)
}

\keyword{file}

