\name{projection}

\alias{projection<-}
\alias{projection}

\title{ projection }

\description{
Get or set the projection of a Raster* object
}

\usage{
projection(x, asText = TRUE)
projection(x) <- value
}

\arguments{
  \item{x}{A Raster* object }
  \item{asText}{logical. If \code{TRUE}, the projection is returned as text. Otherwise a CRS object is returned}
  \item{value}{a CRS object or a character string describing a projection and datum in PROJ4 format }
}


\value{
a Raster* object
}

\seealso{ \code{\link[raster]{projectRaster}, \link[rgdal]{CRS-class}, \link[rgdal]{spTransform-methods}, \link[rgdal]{projInfo}}}

\details{ projections are done by with the PROJ.4 library exposed by rgdal }

\author{ Robert J. Hijmans }

\examples{ 
r <- raster()
projection(r)
projection(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
projection(r)
}

\keyword{ spatial }

