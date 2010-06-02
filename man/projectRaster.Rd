\name{projectRaster}

\alias{projectRaster}
\alias{projectExtent}

\title{project a RasterLayer}

\description{
Change the projection of a RasterLayer. 
\code{projecExtent} returns a RasterLayer with a projected extent, but without any values. This RasterLayer can then 
be used as \code{'to'} RasterLayer in \code{projectRaster}. \code{projectRaster} computes values for the cells of the new RasterLayer.
}

\usage{
projectRaster(from, to, method="ngb", filename="", ...) 
projectExtent(object, crs)
}

\arguments{
  \item{from}{a RasterLayer object}
  \item{to}{a Raster* object}
  \item{method}{method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor) or 'bilinear' (bilinear interpolation).}
  \item{filename}{character. output filename}  
  \item{...}{additional arguments. See Details.}    
  \item{object}{Extent or Raster* object}
  \item{crs}{Character or object of class CRS. PROJ4 type descriptin of the coordinate reference system}

}

\details{
First create a template RasterLayer with a projected extent; also set the number of rows and columns (or the resolution); and perhaps adjust the extent. 
Then use that object to project the input RasterLayer to. 

The resolution of the output RasterLayer should not be much larger of that of the input RasterLayer.

See \code{projInfo('proj')}, \code{projInfo('ellps')}, and \code{projInfo('datum')} for options for valid PROJ.4 strings. 
 
You can consult this page: \url{http://www.remotesensing.org/geotiff/proj_list/} to find the parameter options and names for projections.
 
Projection is performed using the PROJ.4 library accesed through the rgdal package. Also see \code{?CRS}


The following additional arguments can be passed, to replace default values for this function
\tabular{rll}{
  \tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
  \tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeRaster}} \cr
  \tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
  \tab \code{progress} \tab Character. Valid values are "text", "tcltk", "windows" (on that platform only) and ""  \cr
 }
}

\value{
A RasterLayer object, and, in some cases, the side-effect of a raster file written to disk.
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

