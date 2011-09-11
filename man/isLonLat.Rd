\name{isLonLat}

\alias{isLonLat}
\alias{isLonLat,Raster-method}
\alias{isLonLat,CRS-method}
\alias{isLonLat,character-method}

  
\title{Is this longitude/latitude data?}

\description{
 Test whether a Raster* object has the a longitude / latitude coordinate reference system.
}

\usage{
isLonLat(x)
}

\arguments{
\item{x}{Raster* object}
}


\value{
Logical
}

\author{Robert J. Hijmans }

\examples{
r <- raster()
isLonLat(r)
projection(r) <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"
isLonLat(r)
}

\keyword{spatial}
