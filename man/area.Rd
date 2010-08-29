\name{area}

\alias{area}
\alias{area,RasterLayer-method}
\alias{area,RasterStackBrick-method}
  
\title{Size of cells}

\description{
Compute the approximate surface area of cells in an unprojected (longitude/latitude) RasterLayer. It is an approximation because area is computed as the height (latitudial span) 
of a cell (which is constant among all cells) times the width (longitudinal span) in the (latitudinal) middle of a cell. The width is smaller at the polewards side than at 
the equator-wards side of a cell. This variation is greatest near the poles and the values are thus not very precise for very high latitudes. 

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
  \tab \code{na.rm=FALSE}  \tab Logical. If \code{TRUE}, cells that are \code{NA} in \code{x} are ignored\cr
  \tab \code{weights=FALSE}  \tab Logical. If \code{TRUE}, area of each cells is divided by the total area of all cells that are not NA\cr

  \tab \code{filename}  \tab Filename for the output RasterLayer \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{progress} \tab Character. Choose from "text", "window", or "" (the default, no progress bar)  \cr
 }

If \code{x} is a RasterStack/Brick, a RasterBrick will be returned if \code{na.rm=TRUE}. However, if \code{na.rm=FALSE}, a RasterLayer is returned, because the values would be the same for all layers.
}

\value{
a RasterLayer. Cell values represent the size of the cell in km2, or the relative size if \code{weights=TRUE}
}

\author{Robert J. Hijmans }

\examples{

r <- raster(nrow=18, ncol=36)
a <- area(r)
}

\keyword{methods}
\keyword{spatial}
