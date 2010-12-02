\name{cellStats}

\alias{cellStats}

\title{Cell statistics}

\description{
Compute statistics for the cells of each layer of a Raster object. In the \code{raster} package, functions such as max, min, and mean,
when used with Raster objects as argument, return a new Raster object (with a value computed for each cell). In contrast, cellStats returns a single value, computed from the all the values of a layer.
}

\usage{
cellStats(x, stat='mean', ...)
}

\arguments{
 \item{x}{A Raster* object}
 \item{stat}{The function to be applied}
 \item{...}{Additional arguments }  
}

\value{
Numeric.
}

\details{
\code{cellStats} will fail (gracefully) for very large Raster objects except for a number of known functions: sum, mean, min, max, sd, 'countNA'. 'countNA' must be supplied as a character value (with quotes), the other functions may be supplied with quotes. For other functions you could perhaps use a sample of the RasterLayer that can be held in memory (see \code{\link[raster]{sampleRandom}} and \code{\link[raster]{sampleRegular}} )
For large files, and when \code{fun='sd'}, this function returns the standard deviation of the population ( \code{sqrt((1/N) * sum(x^2) - mean(x)^2)}), not that of a sample which is normally returned by \code{sd})
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
}

\keyword{spatial}
\keyword{univar}
