
\name{shapefile}

\alias{shapefile}
\alias{shapefile,character-method}
\alias{shapefile,Spatial-method}

\title{
Read or write a shapefile
}

\description{
Reading and writing of "ESRI shapefile" format spatial data. Only the three vector types (points, lines, and polygons) can be stored in shapefiles. 
These are simple wrapper functions around readOGR and writeOGR (rgdal package).
}


\usage{
\S4method{shapefile}{character}(x, stringsAsFactors=FALSE, verbose=FALSE, ...)

\S4method{shapefile}{Spatial}(x, filename='', overwrite=FALSE, ...)
}
 
\arguments{
  \item{x}{character (a file name, when reading a shapefile) or Spatial* object (when writing a shapefile)}
  \item{filename}{character. Filename to write a shapefile}
  \item{overwrite}{Logical. Overwrite existing shapefile?}
  \item{verbose}{Logical. If \code{TRUE}, information about the file is printed}
  \item{stringsAsFactors}{Logical. If \code{TRUE}, strings are converted to factors}
  \item{...}{Additional arguments passed to rgdal functions readOGR or writeOGR}
}

\value{
Spatial*DataFrame (reading). Nothing is returned when writing a shapefile.
}

\examples{
if (require(rgdal)) {

filename <- system.file("external/lux.shp", package="raster")
filename
p <- shapefile(filename)

\dontrun{
shapefile(p, 'copy.shp')
}
}
}

\keyword{spatial}

