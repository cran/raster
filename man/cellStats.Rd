\name{cellStats}

\alias{cellStats}

\title{Cell statistics}

\description{
Compute statistics for the cells of a single RasterLayer. In the \code{raster} package, functions such as max, min, and mean,
when used with a Raster objects as argument, return a new RasterLayer (with a value computed for each cell). In contrast, cellStats returns a single value, 
computed from the all the values of a layer.
}

\usage{
cellStats(x, stat='mean', ...)
}

\arguments{
 \item{x}{A Raster* object}
 \item{stat}{The function to be applied. Either as character: 'mean', 'min', 'max', 'sum', or 'sd'; or a function (see Details) }
 \item{...}{Additional argument. See \code{\link[raster]{rasterOptions}} }  
}

\value{
A numeric value.
}

\details{
If \code{stat} is a \code{function} rather than a character string, \code{zonal} will fail (gracefully) for very large RasterLayers. In such cases
you can use the character string function name if applicable. Otherwise you could use a sample of the RasterLayer that can be held in memory (see 
\code{\link[raster]{sampleRandom}} and \code{\link[raster]{sampleRegular}} )

\code{stat='sd'} returns slightly different values than \code{stat=sd}, because the former computes the standard deviation of the population (rather than of a sample)
using this formula: \code{sqrt((1/N) * sum(x^2) - mean(x)^2)}
}

\seealso{ \code{\link[raster]{quantile} }, \code{\link[raster]{minValue} }, \code{\link[raster]{maxValue} }, \code{\link[raster]{setMinMax} } }

\author{Robert J. Hijmans}

\examples{
r <- raster(nrow=18, ncol=36)
r[] <- runif(ncell(r)) * 10
# works for large files
cellStats(r, 'mean')
# same, but does not work for very large files
cellStats(r, mean)
# multi-layer object
cellStats(brick(r,r), mean)

# not quite the same
cellStats(r, 'sd')
cellStats(r, sd)
}

\keyword{spatial}
\keyword{univar}
