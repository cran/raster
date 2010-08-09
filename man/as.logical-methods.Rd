\name{as.logical}

\docType{methods}

\alias{as.logical,RasterLayer-method}

\title{ Change values to logical}

\description{
 Change values of a RasterLayer to logical values (zero becomes FALSE, all other values become TRUE)
}

\seealso{ \code{\link[base]{as.logical}} }

\examples{
r <- raster(nrow=10, ncol=10)
r[] <- round(runif(ncell(r)))
r <- as.logical(r)
}


\keyword{methods}
\keyword{spatial}
