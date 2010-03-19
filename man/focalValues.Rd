\name{focalValues}

\docType{methods}

\alias{focalValues}
\alias{focalValues,RasterLayer-method}


\title{ Extract focal values }

\description{
Extract focal values for a single row of a RasterLayer
}

\usage{
focalValues(x, ...)
}

\arguments{
\item{x}{RasterLayer object}
\item{...}{Additional arguments. See Details}
}


\details{
Additional arguments that can be suplied to this function:
1) \code{r}. row number (this is a required argument). 
2) \code{ngb}. The neighborhood to consider. Either a single integer or a vector of two integers. Default is 3. 
}

\value{
A two column matrix. The first column has the column numbers, the second has the focal values. 
}

\seealso{ \code{\link[raster]{cellValues}},  \code{\link[raster]{xyValues}}, \code{\link[raster]{polygonValues}}, \code{\link[raster]{focal}}}

\author{Robert J. Hijmans }

\examples{
x <- raster(nrow=10, ncol=10)
x[] <- 1:ncell(x)
focalValues(x, r=5)
}

\keyword{methods}
\keyword{spatial}
