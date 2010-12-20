\name{flip}

\docType{methods}

\alias{flip}
\alias{flip,RasterLayer-method}
\alias{flip,RasterStackBrick-method}
  
\title{Flip}

\description{
Flip the values of a Raster* object by inverting the order of the rows (direction=y) or the columns direction='x'.
}

\usage{
flip(x, direction, ...)
}

\arguments{
  \item{x}{a Raster* object}
  \item{direction}{Character. 'y' or 'x'}
  \item{...}{Additional arguments, see Details}  
}

\value{
RasterLayer or RasterBrick
}

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{lll}{
  \tab \code{filename} \tab Character. Output filename\cr 
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
 
\code{direction} can also be indicated with 1 (=x) or 2 (=y)
}


\seealso{ transpose: \code{\link{t}}, \code{\link[raster]{rotate}} }

\author{Robert J. Hijmans }

\examples{
r <- raster(nrow=18, ncol=36)
m <- matrix(1:ncell(r), nrow=18)
r[] <- as.vector(t(m))
rx <- flip(r, direction='x')
r[] <- as.vector(m)
ry <- flip(r, direction='y')
}

\keyword{spatial}
