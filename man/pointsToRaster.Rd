\name{pointsToRaster}

\alias{pointsToRaster}

\title{Point to raster conversion}

\description{ 
Use points to set values of a RasterLayer object. For example to count the number of points, or points representing unique classes
}

\usage{
pointsToRaster(raster, xy, values=1, fun=length, background=NA, filename="", ...)
}

\arguments{
  \item{raster}{RasterLayer object}
  \item{xy}{Matrix of x and y coordinates (in the same map reference system as the raster) or a SpatialPoints* object}
  \item{values}{Vector of length of the number of points (i.e., dim(xy)[1] if xy is a matrix) with the values to be used; alternatively you can supply a single value which is then assigned to each point} 
  \item{fun}{Function to be applied, on a cell by cell basis, to the points a cell; the default function is \code{length}, i.e. it returns the number of points in a cell. Typical examples of other functions that can be used are mean, sum, max and \code{function(x)length(unique(x))} to get the number of unique values ('richness')}
  \item{background}{Numeric. The value to be applied to the cells in which no points fall (typically \code{NA} or \code{0})}
  \item{filename}{Character. Output filename}
  \item{...}{Additional arguments. See Details}  
}

\details{
Each point is assinged to a grid cell. The value of a grid cell is determined by the values associated with the points and function fun.
If you want the count of points in each grid cell, use the (default) \code{length} function. I.e. for each cell it computes the length
of the vector of points.
For the sum of the values, use \code{sum}, for a yes/no result, you can use \code{fun=function(x){length(x)>0}}

For the number of unique values use \code{fun=function(x){length(unique(x))}}

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\value{
  RasterLayer object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{linesToRaster}}, \code{\link[raster]{polygonsToRaster}}}

\examples{
r <- raster(ncols=36, nrows=18)
n <- 1000
x <- runif(n)* 360 - 180
y <- runif(n)* 180 - 90
xy <- cbind(x, y)
vals <- rep(1, n)
r <- pointsToRaster(r, xy, vals)

# now with an sp SpatialPointsDataFrame object
p <- as.data.frame(cbind(xy, vals))
coordinates(p) <- ~x+y
r <- pointsToRaster(r, p, p@data[,'vals'])
}

\keyword{spatial}
