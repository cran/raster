\name{cellValues}
\docType{methods}

\alias{cellValues}
\alias{cellValues,RasterLayer,vector-method}
\alias{cellValues,RasterBrick,vector-method}
\alias{cellValues,RasterStack,vector-method}

\title{ Extract values for one or more cells }

\description{
These methods return values of a Raster* object for the cells specified  )
}

\usage{cellValues(x, cells)}

\arguments{
 \item{x}{RasterLayer or RasterStack object}
 \item{cells}{vector of cell numbers, cell numbers should be between 1 and ncells(x)}
}

\value{
a vector of cell values for a RasterLayer or or a matrix of values for a RasterStack or RasterBrick
}

\seealso{ \code{\link[raster]{xyValues}}, \code{\link[raster]{read}} }


\author{Robert J. Hijmans }


\examples{
r <- raster(ncol=10, nrow=10)
r <- setValues(r, 1:ncell(r))
cellValues(r, c(1, 10, 100))

s <- stack(r, sqrt(r))
cellValues(s, c(1, 10, 100))
}


\keyword{methods}
\keyword{spatial}
