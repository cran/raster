\name{cover}

\docType{methods}

\alias{cover}
\alias{cover,RasterLayer,RasterLayer-method}
\alias{cover,RasterStackBrick,Raster-method}

\title{ Cover }
\description{
 Replace \code{NA} values in the first Raster object (\code{x}) with the values of the second (\code{y}), and so forth for additional Rasters.
 If \code{x} has mutliple layers, the subsequent Raster objects should have the same number of layers, or have a single layer only (which will be recycled).
}


\section{Methods}{
\describe{
\code{cover(x, y, ..., filename="") }
  \item{\code{x}}{ Raster* object }
  \item{\code{y}}{ Raster* object }
  \item{\code{...}}{ Additional Raster* objects or other arguments (see Details) }
  \item{\code{filename}}{ filename for the output RasterLayer}

Additional arguments for writing the results to a file on disk:
\tabular{rll}{
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
  
}}


\value{
A RasterLayer or RasterBrick object, and in some cases the side effect of a new file on disk.
}

\author{Robert J. Hijmans}

\examples{
r1 <- raster(ncols=36, nrows=18)
r1[] <- 1:ncell(r1)
r2 <- setValues(r1, runif(ncell(r1)))
r2[r2<0.5] <- NA
r3 <- cover(r2, r1)
}	

\keyword{methods}
\keyword{spatial}
