\name{changeStack}

\alias{addLayer}
\alias{addLayer,RasterStack-method}
\alias{addLayer,RasterBrick-method}
\alias{addFiles}
\alias{dropLayer}
\alias{dropLayer,RasterStack-method}
\alias{dropLayer,RasterBrick-method}

\title{Add or drop a layer}

\description{
  A RasterStack is a collection of RasterLayers with the same spatial extent and resolution. A RasterBrick is a multi-layer object.
  With these functions, you can add RasterLayers to, or remove RasterLayers from, a RasterStack or RasterBrick object. 
}

\usage{
addLayer(x, ...) 
addFiles(x, rasterfiles, bands= rep(1, length(rasterfiles))) 
dropLayer(x, i) 
}

\arguments{
  \item{x}{ a RasterStack or RasterBrick object }
  \item{...}{Raster* objects or filenames (character) }
  \item{rasterfiles}{ Filename(s) of (a) raster dataset(s) }
  \item{bands}{ a vector or list of bands of raster data files (default values = 1)}
  \item{i}{ a vector of the indices of the layer(s) to remove from a RasterStack }  
}


\value{
  a RasterStack object
}
\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}}}

\examples{
  rasterfile <- system.file("external/test.grd", package="raster")
  st <- stack(rasterfile)
# now adding the same file multiple times, in most cases you would add different files  
# this generates warnings, but it is not an error
  st <- addFiles(st, c(rasterfile, rasterfile))
  rs <- raster(rasterfile)
  st <- addLayer(st, c(rs, rs))
  st
  st <- dropLayer(st, c(3, 5))
  nlayers(st)
}

\keyword{ spatial }
