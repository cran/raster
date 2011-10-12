\name{merge}

\docType{methods}

\alias{merge}
\alias{merge,Raster,Raster-method}
\alias{merge,list,missing-method}

\title{ 
Merge
}

\description{
Merge Raster* objects to form a new Raster object with a larger spatial extent. 
}

\usage{
merge(x, y, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{y}{A Raster* object with the same number of layers as \code{x}}
  \item{...}{Additional Raster* objects and other arguments. See below, under Methods}  
}


\section{Methods}{
\describe{
A full call to merge is:

\code{merge(x, y, ..., tolerance=0.05, filename="", overwrite, format, progress ) }

  \item{\code{x}}{Raster* object}
  \item{\code{y}}{Raster* object}
  \item{\code{...}}{Additional RasterLayer objects}
  \item{\code{tolerance}}{Numeric. Difference permissable (relative to the cell resolution) for objects to be 'equal'. See ?all.equal}
  \item{\code{filename}}{Character. output filename}
  \item{\code{overwrite}}{Logical. If \code{TRUE}, "filename" will be overwritten if it exists}
  \item{\code{format}}{Character. output file type. Either 'raster', 'ascii' or a supported GDAL 'driver' name see \code{\link[raster]{writeFormats}}}
  \item{\code{datatype}}{Character. Output data type. See \code{\link[raster]{dataType}}}
  \item{\code{progress}}{Character. "text", "window", or "" (the default, no progress bar)}
}  
} 

\details{
The Raster objects must have the same origin and resolution. In areas where the Raster objects overlap, the values of the Raster object that is first in the sequence of arguments will be retained. If you'd rather use the average of cell values, or do another computation, you can use \code{\link[raster]{mosaic}} instead of merge.
}

\value{
A RasterLayer or RasterBrick object, and, in some cases, the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{mosaic}}, \code{\link[raster]{crop}}, \code{\link[raster]{expand}}}

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

