\name{flip}

\docType{methods}

\alias{flip}
\alias{flip,RasterLayer-method}
  
\title{Flip}

\description{
Flip a RasterLayer by inverting the order of the rows (direction=y) or the columns direction='x'.
}

\usage{
flip(x, direction, ...)
}

\arguments{
  \item{x}{a RasterLayer object}
  \item{direction}{Character. 'y' or 'x'}
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
 
\code{direction} can also be indicated with 1 (=x) or 2 (=y)
}

\author{Robert J. Hijmans }

\seealso{ \code{\link[raster]{rotate}} }


\examples{
r <- raster(nrow=18, ncol=36)
m <- matrix(1:ncell(r), nrow=18)
r[] <- as.vector(t(m))
rx <- flip(r, direction='x')
r[] <- as.vector(m)
ry <- flip(r, direction='y')
}

\keyword{spatial}
