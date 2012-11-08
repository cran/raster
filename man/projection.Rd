\name{projection}

\alias{projection}
\alias{projection<-}
\alias{proj4string}
\alias{proj4string<-}

\title{ Get or set a coordinate reference system (projection) }

\description{
Get or set the coordinate reference system (CRS) of a Raster* object. 
}

\usage{
projection(x, asText=TRUE)
projection(x) <- value
}

\arguments{
  \item{x}{A Raster* object }
  \item{asText}{logical. If \code{TRUE}, the projection is returned as text. Otherwise a \code{\link[sp]{CRS}} object is returned}
  \item{value}{a \code{CRS} object or a character string describing a projection and datum in the PROJ.4 format }
}


\value{
Raster* object
}

\note{
For compatability with \code{sp} you can use \code{proj4string} instead of \code{projection}. 
}

\seealso{ \code{\link[raster]{projectRaster}, \link[rgdal]{CRS-class}, \link[rgdal]{spTransform-methods}, \link[rgdal]{projInfo}}}

\details{ projections are done by with the PROJ.4 library exposed by rgdal }



\examples{ 
r <- raster()
projection(r)
projection(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
projection(r)
}

\keyword{ spatial }

