\name{as.logical}

\docType{methods}

\alias{as.logical,Raster-method}

\title{ Change values to logical}

\description{
 Change values of a Raster* object to logical values (zero becomes \code{FALSE}, all other values become \code{TRUE})
 You can provide the standard additional arguments: filename, format, overwrite, and progress. 
}

\seealso{ \code{\link[base]{as.logical}} }

\examples{
r <- raster(nrow=10, ncol=10)
r[] <- round(runif(ncell(r)))
r <- as.logical(r)
}


\keyword{methods}
\keyword{spatial}
