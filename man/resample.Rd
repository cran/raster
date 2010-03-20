\name{resample}
\alias{resample}

\title{Resample}

\description{
Resample transfers values between non matching RasterLayers (in terms of origin and resolution). 

Avoid using this function if you can use functions like (dis)aggregate, crop and merge instead, because these functions are more accurate and much faster.
}

\usage{
resample(from, to, method="ngb", filename="", ...) 
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a Raster* object}
  \item{method}{method used to compute values for the new RasterLayer}
  \item{filename}{character. output filename}  
  \item{...}{additional arguments. See Details.}    
 }

\details{
valid values for \code{method} are \code{'ngb'} (nearest neighbor) and \code{'bilinear'} (bilinear interpolation).


The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{
A RasterLayer object, and, in some cases, the side-effect of a file with the values of the new RasterLayer 
}

\author{Robert J. Hijmans }


\seealso{ \code{\link[raster]{aggregate}}, \code{\link[raster]{disaggregate}}, \code{\link[raster]{crop}}, , \code{\link[raster]{expand}}, \code{\link[raster]{merge}}, \code{\link[raster]{projectRaster}} }


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

