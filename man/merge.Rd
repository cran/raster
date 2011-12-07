\name{merge}

\docType{methods}

\alias{merge}
\alias{merge,Raster,Raster-method}

\title{ 
Merge
}

\description{
Merge Raster* objects to form a new Raster object with a larger spatial extent. If objects overlap, the values get priority in the same order as the arguments, but \code{NA} values are ignored (except when \code{overlap=FALSE})

See the \code{\link{merge,Spatial,data.frame-method}} for merging a data.frame with attributes of Spatial objects and the \code{\link{merge,SpatialPolygons,SpatialPolygons-method}} for appending and perhaps spatially intersecting polygons.
}

\usage{
\S4method{merge}{Raster,Raster}(x, y, ..., tolerance=0.05, filename="", format, datatype, overwrite, progress, overlap=TRUE, ext=NULL)
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{Raster* object}
  \item{...}{Additional Raster objects}
  \item{tolerance}{Numeric. Permissable difference in origin (relative to the cell resolution). See \code{\link[base]{all.equal}}}
  \item{filename}{Character. output filename}
  \item{overwrite}{Logical. If TRUE, "filename" will be overwritten if it exists}
  \item{format}{Character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeFormats}}}
  \item{datatype}{Character. Output data type. See \code{\link[raster]{dataType}}}
  \item{progress}{Character. "text", "window", or "" (the default, no progress bar)}
  \item{overlap}{Logical. If \code{FALSE} values of overlapping objects are based on the first layer, even if they are \code{NA}}
  \item{ext}{Extent object (optional) to limit the output to that extent}
} 

\details{
The Raster objects must have the same origin and resolution. In areas where the Raster objects overlap, the values of the Raster object that is first in the sequence of arguments will be retained. If you'd rather use the average of cell values, or do another computation, you can use \code{\link[raster]{mosaic}} instead of merge.
}

\value{
RasterLayer or RasterBrick
}

\author{Robert J. Hijmans}

\seealso{
The \code{\link{merge,Spatial,data.frame-method}} for merging a data.frame with attributes of Spatial objects

The \code{\link{merge,SpatialPolygons,SpatialPolygons-method}} for appending and perhaps spatially intersecting polygons
}

\examples{
r1 <- raster(xmx=-150, ymn=60, ncols=30, nrows=30)
r1[] <- 1:ncell(r1)
r2 <- raster(xmn=-100, xmx=-50, ymx=50, ymn=30)
res(r2) <- c(xres(r1), yres(r1))
r2[] <- 1:ncell(r2)
rm <- merge(r1, r2)
}

\keyword{methods}
\keyword{spatial}

