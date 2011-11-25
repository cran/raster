\name{shift}

\docType{methods}

\alias{shift}
\alias{shift,Raster-method}
  
\title{Shift}

\description{
Shift the location of a Raster* object in the x and/or y direction
}

\usage{
shift(object, ...)
}

\arguments{
  \item{object}{A Raster* object}
  \item{...}{Additional arguments, see Details}  
}
 
\value{
a Raster* object
}


\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{lll}{
  \tab \code{x} \tab Numeric. The shift in horizontal direction \cr
  \tab \code{y} \tab Numeric. The shift in vertical direction \cr
  \tab \code{filename} \tab Character. Output filename\cr 
  \tab \code{format} \tab Character. Output file type. See \link[raster]{writeRaster} \cr
  \tab \code{datatype} \tab Character. Output data type. See \link[raster]{dataType} \cr
  \tab \code{overwrite} \tab Logical. If TRUE, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}
  
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{flip}}, \code{\link[raster]{rotate}} }

\examples{
r <- raster()
r <- shift(r, x=1, y=-1)
}

\keyword{spatial}
