\name{rotate}

\docType{methods}

\alias{rotate}
\alias{rotate,RasterLayer-method}

  
\title{Rotate}

\description{
Rotate a RasterLayer that has x coordinates (longitude) from 0 to 360, to standard coordinates between -180 and 180 degrees. Longitude between 0 and 360 is frequently used in data from global climate models.
}

\usage{
rotate(x, ...)
}

\arguments{
  \item{x}{a RasterLayer object}
  \item{...}{Additional arguments, see Details}  
}

\value{
a RasterLayer
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{lll}{
  \tab \code{filename} \tab Character. Output filename\cr 
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\seealso{ \code{\link[raster]{flip}} }

\author{Robert J. Hijmans }
\examples{

r <- raster(nrow=18, ncol=36)
m <- matrix(1:ncell(r), nrow=18)
r[] <- as.vector(t(m))
extent(r) <- extent(0, 360, -90, 90)
rr <- rotate(r)
}

\keyword{spatial}
