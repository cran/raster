\name{aggregate}

\docType{methods}

\alias{aggregate}
\alias{aggregate,RasterLayer-method}
\alias{aggregate,RasterStackBrick-method}

\title{Aggregate cells}

\description{
Aggregate a Raster* object to create a new RasterLayer or RasterBrick with a lower resolution (larger cells). 
Aggregation groups rectangular areas to create larger cells. 
The value for the resulting cells is computed with a user-specified function (default value = \code{mean}). 
}

\usage{
aggregate(x, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{...}{Additional arguments. See under Details}  
}

\details{
 A full call to the aggregate method for a Raster* object is:

\code{aggregate(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename="", ... ) }

\tabular{rll}{
\tab \code{x} \tab a RasterLayer object \cr
\tab \code{fact} \tab Integer. Aggregation factor expressed as number of cells in each direction (horizontally and vertically). 
Or two integers (horizontal and vertical aggregation factor). Default is 2. See below \cr
\tab \code{fun} \tab Function used to aggregate values (default=mean)\cr
\tab \code{expand} \tab logical. If \code{TRUE} the output RasterLayer will be larger then the input RasterLayer if a division
 of the number of columns or rows with \code{factor} is not an integer \cr
\tab \code{na.rm} \tab logical. If \code{TRUE}, NA cells are removed from calculations \cr
\tab \code{filename} \tab Character. Output filename \cr
\tab \code{...} \tab Additional arguments, see below \cr
}

Additional arguments:
\tabular{rll}{
\tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
\tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
\tab \code{overwrite} \tab Logical. If \code{TRUE}, the file will be overwritten if it exists \cr
\tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
}

Aggregation will result in a Raster* object with \code{fact*fact} fewer cells; if necessary this number is adjusted according to the value of \code{expand}.
For example, \code{fact=2} will result in a new Raster* object with \code{2*2=4} times fewer cells. If two numbers are supplied, e.g., \code{fact=c(2,3)}, the first will be used for aggregating in the horizontal direction, 
and the second for aggregating in the vertical direction, and the new RasterLayer will have \code{2*3=6} times fewer cells.  
 
Aggregation starts at the upper-left end of a raster. If a division of the number of columns or rows with \code{factor} does not 
return an integer, the extent of the resulting RasterLayer will either be somewhat smaller or somewhat larger then the original RasterLayer.
For example, if an input RasterLayer has 100 columns, and \code{fact=12}, the output RasterLayer will have either 8 columns (\code{expand=FALSE}) 
(using \code{8 x 12 = 96} of the original columns) or 9 columns (\code{expand=TRUE}). In both cases, the maximum x coordinate of the output RasterLayer would, of course, also be adjusted.
  
The function \code{fun} should take multiple numbers, and return a single number. For example \code{mean}, \code{modal}, \code{min} or \code{max}. 
It should also accept a \code{na.rm} argument (or ignore it as one of the 'dots' arguments). 
}

\value{
RasterLayer or RasterBrick
}

\seealso{ \code{\link{disaggregate}, \link{resample}} }

\author{Robert J. Hijmans and Jacob van Etten}

\examples{
r <- raster()
# a new aggregated raster, no values
ra <- aggregate(r, fact=10)
r <- setValues(r, runif(ncell(r)))
ra <- aggregate(r, fact=10, fun=max)
# a new aggregated raster, max of the values
}

\keyword{methods}
\keyword{spatial}

