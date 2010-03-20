\name{area}

\alias{area}
\alias{area,Raster-method}
  
\title{Size of cells}

\description{
Compute the approximate surface area of cells in an unprojected (longitude/latitude) RasterLayer. It is an approximation as area is computed as the height of a cell (which is constant among all cells) times the width in the middle of a cell (the width is smaller at the polewards side than at the equator-wards side of a cell). This variation is greatest near the poles and the values are thus not very precise for very high latitudes. 
}

\usage{
area(x, ...)
}

\arguments{
\item{x}{a Raster* object}
\item{...}{Additional arguments, see Details}
}


\details{
The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
    \tab \code{filename}  \tab Filename for the output RasterLayer \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{
a RasterLayer. Cell values represent the size of the cell in km2
}

\author{Robert J. Hijmans }

\examples{

r <- raster(nrow=18, ncol=36)
a <- area(r)
}

\keyword{methods}
\keyword{spatial}
