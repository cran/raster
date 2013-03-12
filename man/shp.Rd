
\name{shapefile}

\alias{shapefile}

\title{
Read or write a shapefile
}

\description{
Reading and writing "shapefiles". Simple wrapper around readOGR and writeOGR (rgdal package).
}


\usage{
shapefile(filename, object=NULL, overwrite=FALSE, verbose=FALSE, stringsAsFactors=FALSE, ...)
}
 
\arguments{
  \item{filename}{Character. Full filename of a shapefile}
  \item{object}{NULL when reading a shapefile. A Spatial* object when writing a shapefile}
  \item{overwrite}{Logical. Overwrite existing shapefile?}
  \item{verbose}{Logical. If \code{TRUE}, information about the file is printed}
  \item{stringsAsFactors}{Logical. If \code{TRUE}, strings are converted to factors}
  \item{...}{Additional arguments passed to rgdal functions readOGR or writeOGR}
}

\value{
Spatial*DataFrame (reading). Nothing is returned when writing a shapefile.
}



\keyword{spatial}

