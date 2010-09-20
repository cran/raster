\name{pointsToRaster}

\alias{pointsToRaster}

\title{Point to raster conversion}

\description{ 
Use points to set values of a RasterLayer object. For example to count the number of points, or points representing unique classes
}

\usage{
pointsToRaster(raster, xy, values=1, fun=NULL, background=NA, na.rm=TRUE, filename="", ...)
}

\arguments{
  \item{raster}{RasterLayer object}
  \item{xy}{Matrix of x and y coordinates (in the same map reference system as the raster) or a SpatialPoints* object}
  \item{values}{Vector of length of the number of points (i.e., dim(xy)[1] if xy is a matrix) with the values to be used; alternatively 
  you can supply a single value which is then assigned to each point. You can also supply a matrix if its number of rows matches the number of points. If the matrix has more than one column, a RasterBrick will be returned, with a layer for each column in \code{values}. }
  
  \item{fun}{Function to be applied, on a cell by cell basis, to the (values of) the points that fall in a cell. If no function is specified, the number of points per cell is returned. Typical examples of functions that can be used are \code{mean}, \code{sum}, \code{min} and \code{max}. Note that the function must take an \code{na.rm} argument, either explicitly or through 'dots'. This 
  \code{fun=lenght} fails, but \code{fun=function(x,...)lenght(x)} works (but ignores the \code{na.rm} argument. To use the \code{na.rm} argument you can use a function like this \code{fun=function(x,na.rm){if (na.rm) length(na.omit(x)) else (length(x)}}, or use a function that removes NA values in all cases, like this functions for compute the number of unique values "richness": \code{fun=function(x, ...){length(unique(na.omit(x)))}}. 
  You can also pass multiple functions using a statement like \code{fun=function(x)c(lenght(x),mean(x))}, in which case the returned object is a RasterBrick (multiple layers).}
  
  \item{background}{Numeric. The value to be applied to the cells in which no points fall (typically \code{NA} or \code{0})}
  \item{na.rm}{Logical. Remove \code{NA} values? This argument might be ignored by \code{fun} }
  \item{filename}{Character. Output filename}
  \item{...}{Additional arguments. See Details}  
}

\details{
Each point is assinged to a grid cell. The value of a grid cell is determined by the values associated with the points and function fun.
If you want to know the number of points in each grid cell, use the (default) \code{length} function. I.e., for each cell it computes the length
of the vector of points. For the sum of the values, use \code{sum}, for a yes/no result, you can use \code{fun=function(x){length(x)>0}}

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

\author{Robert J. Hijmans, Steven Mosher & Paul Hiemstra}

\seealso{ \code{\link[raster]{linesToRaster}}, \code{\link[raster]{polygonsToRaster}},  \code{\link[raster]{rasterFromXYZ}} }

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
