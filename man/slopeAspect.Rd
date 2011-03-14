\name{Slope and aspect}

\alias{slopeAspect}

\title{Slope and aspect}

\description{
Compute slope and/or aspect from elevation data. Elevation data should be in map units (typically meter for projected rasters), or in meters when the CRS is longitude/latitude. 
}

\usage{
slopeAspect(alt, filename='', type='', unit='', ...)  
}

\arguments{
  \item{alt}{RasterLayer object with elevation values in map units, or in meters when the crs is longitude/latitude}
  \item{filename}{Character. Filename. optional}
  \item{type}{Character. 'slope', 'aspect', or 'both' (=default, slope and aspect)}
  \item{unit}{Character. 'degrees' or '' (=default, radians for aspect, and fraction for slope) }
  \item{...}{Standard additional arguments for writing RasterLayer files}
}


\author{Robert J. Hijmans}


\keyword{spatial}
