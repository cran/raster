\name{mosaic}

\docType{methods}

\alias{mosaic}
\alias{mosaic,RasterLayer,RasterLayer-method}

\title{ 
mosaic RasterLayers
}

\description{
Mosaic RasterLayers to form a new RasterLayer with a larger spatial extent applying a function to compute cell values in areas where layers overlap (in contrast to the \code{\link[raster]{merge}} function  which uses the values of the 'upper' layer)
}

\usage{
mosaic(x, y, ...) 
}

\arguments{
  \item{x}{A RasterLayer object}
  \item{y}{A RasterLayer object}
  \item{...}{Additional RasterLayers and other arguments. See below, under Methods}  
}


\section{Methods}{
\describe{
A full call to mosaic is:

\code{mosaic(x, y, ..., fun, na.rm=TRUE, tolerance=0.05, filename="", overwrite, format, progress ) }

  \item{\code{x}}{RasterLayer object}
  \item{\code{y}}{RasterLayer object}
  \item{\code{...}}{Additional RasterLayer objects}
  \item{\code{fun}}{Function. E.g. mean, min, or max. Must be a function that accepts a na.rm argument}
  \item{\code{na.rm}}{Logical. Only return \code{NA} when all values are \code{NA}}
  \item{\code{tolerance}}{Numeric. Difference permissable (relative to the cell resolution) for objects to be 'equal'. See ?all.equal}
  \item{\code{filename}}{Character. output filename}
  \item{\code{overwrite}}{Logical. If \code{TRUE}, "filename" will be overwritten if it exists}
  \item{\code{format}}{Character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeFormats}}}
  \item{\code{datatype}}{Character. Output data type. See \code{\link[raster]{dataType}}}
  \item{\code{progress}}{Character. "text", "window", or "" (the default, no progress bar)}
}  
} 

\details{
The RasterLayer objects must have the same origin and resolution. 
}

\value{
A new RasterLayer object, and in some cases, the side effect of a new file on disk.
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

m = mosaic(r1, r2, r3, fun=mean)
}

\keyword{methods}
\keyword{spatial}

