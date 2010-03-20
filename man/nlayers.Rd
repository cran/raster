\name{nlayers}

\alias{nlayers}
\alias{nlayers,BasicRaster-method}
\alias{nlayers,Raster-method}
\alias{nlayers,RasterStack-method}
\alias{nlayers,RasterBrick-method}
\alias{nlayers,Spatial-method}
\alias{layerNames}
\alias{layerNames<-}
  
\title{Layers}

\description{
Get or set layer properties of a RasterStack object
}

\usage{
nlayers(object)
layerNames(object)
layerNames(object) <- value
}

\arguments{
  \item{object}{Raster* object}
  \item{value}{a vector of character}
}

\value{
nlayers: A single numeric value >= 1
layerNames: a vector of character
}

\seealso{ \code{\link[raster]{bands}} }


\author{Robert J. Hijmans }
\examples{
#using a new default raster (1 degree global)
r <- raster(ncols=10, nrows=10)
r[] <- 1:ncell(r)
s <- stack(r, r, r)
nlayers(s)
layerNames(s)
layerNames(s) <- c('a', 'b', 'c')
layerNames(s)[2] <- c('bb')
}

\keyword{spatial}
