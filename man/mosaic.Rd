\name{mosaic}

\docType{methods}

\alias{mosaic}
\alias{mosaic,Raster,Raster-method}
\alias{mosaic,list,missing-method}

\title{ 
Merge Raster* objects, using a function to compute cell value for overlapping areas.
}

\description{
Mosaic Raster* objects to form a new object with a larger spatial extent. A functions is used to compute cell values in areas where layers overlap (in contrast to the \code{\link[raster]{merge}} function  which uses the values of the 'upper' layer). All objects must have the same origin, resolution, and coordinate reference system.
}

\usage{
mosaic(x, y, ...) 
}

\arguments{
  \item{x}{Raster* object}
  \item{y}{Raster* object}
  \item{...}{Additional RasterLayers and other arguments. See below, under Methods}  
}


\section{Methods}{
\describe{
A full call to mosaic is:

\code{mosaic(x, y, ..., fun, na.rm=TRUE, tolerance=0.05, filename="", overwrite, format, progress ) }

  \item{\code{x}}{Raster* object; or a list of Raster* objects}
  \item{\code{y}}{Raster* object}
  \item{\code{...}}{Additional Raster* objects}
  \item{\code{fun}}{Function. E.g. mean, min, or max. Must be a function that accepts a 'na.rm' argument}
  \item{\code{na.rm}}{Logical. Only return \code{NA} when all values are \code{NA}}
  \item{\code{tolerance}}{Numeric. Permissable difference in origin (relative to the cell resolution). See \link[base]{all.equal}}
  \item{\code{filename}}{Character. output filename}
  \item{\code{overwrite}}{Logical. If TRUE, "filename" will be overwritten if it exists}
  \item{\code{format}}{Character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeFormats}}}
  \item{\code{datatype}}{Character. Output data type. See \link[raster]{dataType}}
  \item{\code{progress}}{Character. "text", "window", or "" (the default, no progress bar)}
}  
} 

\details{
The RasterLayer objects must have the same origin and resolution. 
}

\value{
RasterLayer or RasterBrick object.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{merge}}, \code{\link[raster]{expand}}}

\examples{
r <- raster(ncol=100, nrow=100)
r1 <- crop(r, extent(-10, 11, -10, 11))
r2 <- crop(r, extent(0, 20, 0, 20))
r3 <- crop(r, extent(9, 30, 9, 30))

r1[] <- 1:ncell(r1)
r2[] <- 1:ncell(r2)
r3[] <- 1:ncell(r3)

m1 <- mosaic(r1, r2, r3, fun=mean)

s1 <- stack(r1, r1*2)
s2 <- stack(r2, r2/2)
s3 <- stack(r3, r3*4)
m2 <- mosaic(s1, s2, s3, fun=min)

}

\keyword{methods}
\keyword{spatial}

