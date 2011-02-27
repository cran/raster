\name{resample}

\alias{resample}
\alias{resample,Raster,Raster-method}


\title{Resample a Raster object}


\description{
Resample transfers values between non matching Raster* objects (in terms of origin and resolution). Use \code{\link[raster]{projectRaster}} if the target has a different projection.
Before using resample, you may want to consider using these other functions instead: \code{\link[raster]{aggregate}}, \code{\link[raster]{disaggregate}}, \code{\link[raster]{crop}}, \code{\link[raster]{expand}}, \code{\link[raster]{merge}}.
}


\usage{
resample(x, y, ...) 
}

\arguments{
  \item{x}{Raster* object to be resampled}
  \item{y}{Raster* object with parameters that \code{x} should be resample to}
  \item{...}{additional arguments. See Details.}    
 }

\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{method} \tab method used to compute values for the new RasterLayer, should be "bilinear" for bilinear interpolation, or "ngb" for nearest neighbor \cr
  \tab \code{filename} \tab character. output filename \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\value{
A RasterLayer or RasterBrick object, and, in some cases, the side-effect of a file with the values of the new RasterLayer 
}

\author{Robert J. Hijmans }


\seealso{ \code{\link[raster]{aggregate}}, \code{\link[raster]{disaggregate}}, \code{\link[raster]{crop}}, \code{\link[raster]{expand}}, \code{\link[raster]{merge}}, \code{\link[raster]{projectRaster}} }


\examples{
r <- raster(nrow=3, ncol=3)
r[] <- 1:ncell(r)
s <- raster(nrow=10, ncol=10)
s <- resample(r, s, method='bilinear')
#par(mfrow=c(1,2))
#plot(r)
#plot(s)
}

\keyword{spatial}

