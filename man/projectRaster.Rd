\name{projectRaster}

\alias{projectRaster}
\alias{projectExtent}

\title{project a RasterLayer}

\description{
Project the values of a RasterLayer to a new RasterLayer with another coordinate reference system. 

\code{projecExtent} returns a RasterLayer with a projected extent, but without any values. This RasterLayer can then 
be used as \code{'to'} RasterLayer in \code{projectRaster}. \code{projectRaster} computes values for the cells of the new RasterLayer.
}

\usage{
projectRaster(from, to, method="ngb", filename="", ...) 
projectExtent(object, crs)
}

\arguments{
  \item{from}{a Raster* object}
  \item{to}{a Raster* object}
  \item{method}{method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor) or 'bilinear' (bilinear interpolation).}
  \item{filename}{character. output filename}  
  \item{...}{additional arguments. See Details.}    
  \item{object}{Extent or Raster* object}
  \item{crs}{Character or object of class CRS. PROJ4 type description of the coordinate reference system}

}

\details{
First create a template RasterLayer with an extent in the coordinate system of the crs you want to project to. You can use \code{projectExtent} for this
or an existing Raster* object.
Also set the number of rows and columns (or the resolution), and perhaps adjust the extent. The resolution of the output raster should normally be similar to that of the input raster.
Then use that object to project the input RasterLayer to. 

Projection is performed using the PROJ.4 library accesed through the rgdal package. 

The best place to find PROJ4 coordinate reference system descriptions is \url{http://www.spatialreference.org}. 

You can also consult this page: \url{http://www.remotesensing.org/geotiff/proj_list/} to find the parameter options and names for projections.

Also see \code{projInfo('proj')}, \code{projInfo('ellps')}, and \code{projInfo('datum')} for valid PROJ.4 values. 

The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. "text", "window", or "" (the default, no progress bar)  \cr
 }
}

\note{
Vector (points, lines, polygons) can be transformed with \code{\link[rgdal]{spTransform}}.
}

\value{
A RasterLayer or RasterBrick object, and, in some cases, the side-effect of a raster file written to disk.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[rgdal]{CRS-class}}, \code{\link[rgdal]{projInfo}}}

\examples{
# create a new (not projected) RasterLayer with cellnumbers as values
r <- raster(xmn=-110, xmx=-90, ymn=40, ymx=60, ncols=40, nrows=40)
r <- setValues(r, 1:ncell(r))
# proj.4 projection description
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"

# create a new RasterLayer with a projected extent (no values are transferred)
if (require(rgdal)) {
pr <- projectExtent(r, newproj)
# Adjust the cell size 
res(pr) <- 200000

# project the values of RasterLayer 'r' to the new RasterLayer 'projras'
pr <- projectRaster(r, pr)
# inverse projection, back to the properties of 'r'
inv <- projectRaster(pr, r)

## using a higher resolution and bilinear interpolation
# res(pr) <- 10000
# pr <- projectRaster(r, pr, method='bilinear')
# inv <- projectRaster(pr, r, method='bilinear')
# dif <- r - inv
# plot(dif)
}
}

\keyword{spatial}

