\name{projectRaster}

\alias{projectRaster}
\alias{projectExtent}

\title{Project a Raster object}

\description{
Project the values of a Raster* object to a new Raster* object with another projection (coordinate reference system, (CRS)). 
You can do this by providing the new projection as a single argument in which case the function sets the extent and resolution of the new object. To have more control over the transformation, and, for example, to assure that the new object lines up with other datasets, you can provide a Raster* object with the properties that the input data should be projected to.

\code{projecExtent} returns a RasterLayer with a projected extent, but without any values. This RasterLayer can then 
be adjusted (e.g. by setting its resolution) and used as a template \code{'to'} Raster object in \code{projectRaster}. 
\code{projectRaster} computes values for the cells of the new Raster object.
}

\usage{
projectRaster(from, to, res, crs, method="bilinear", filename="", ...) 
projectExtent(object, crs)
}

\arguments{
  \item{from}{Raster* object}
  \item{to}{Raster* object with the parameters to which 'from' should be projected}
  \item{res}{Single or (vector of) two numerics. To, optionally, set the output resolution if 'to' is missing}
  \item{crs}{Character or object of class 'CRS'. PROJ.4 description of the coordinate reference system. In projectRaster this is used to set the output CRS if 'to' is missing, or if 'to' has no valid CRS}
  \item{method}{Method used to compute values for the new RasterLayer. Either 'ngb' (nearest neighbor), which is useful for categorical variables, or 'bilinear' (bilinear interpolation; the default value), which is appropriate for continuous variables.}
  \item{filename}{Character. Output filename}  
  \item{...}{Additional arguments. See Details.}    
  \item{object}{Raster* object}

}

\details{
There are two approaches you can follow to project the values of a Raster object.

1) Provide a \code{crs} argument, and, optionally, a \code{res} argument, but do not provide a \code{to} argument.

2) Create a template Raster with the CRS you want to project to. You can use an existing object, or use \code{projectExtent} for this
or an existing Raster* object. Also set the number of rows and columns (or the resolution), and perhaps adjust the extent. The resolution of the output raster should normally be similar to that of the input raster. Then use that object as \code{from} argument to project the input Raster to. 
This is the preferred method because you have most control. For example you can assure that the resulting Raster object lines up with other Raster objects.

Projection is performed using the PROJ.4 library accesed through the rgdal package. 

One of the best places to find PROJ.4 coordinate reference system descriptions is \url{http://www.spatialreference.org}. 

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
RasterLayer or RasterBrick object.
}

\author{Robert J. Hijmans}

\seealso{ \code{resample} \code{\link[rgdal]{CRS-class}}, \code{\link[rgdal]{projInfo}}, \code{\link[rgdal]{spTransform}}}


\examples{
# create a new (not projected) RasterLayer with cellnumbers as values
r <- raster(xmn=-110, xmx=-90, ymn=40, ymx=60, ncols=40, nrows=40)
r <- setValues(r, 1:ncell(r))
projection(r)
# proj.4 projection description
newproj <- "+proj=lcc +lat_1=48 +lat_2=33 +lon_0=-100 +ellps=WGS84"

# we need to have rgdal installed for this
if (require(rgdal)) {

#simplest approach
pr1 <- projectRaster(r, crs=newproj)

# alternatively also set the resolution
pr2 <- projectRaster(r, crs=newproj, res=20000)

# to have more control, provide an existing Raster object, here we create one
# using projectExtent (no values are transferred)
pr3 <- projectExtent(r, newproj)
# Adjust the cell size 
res(pr3) <- 200000

# project the values of RasterLayer 'r' to the new RasterLayer 'projras'
pr3 <- projectRaster(r, pr3)

# inverse projection, back to the properties of 'r'
inv <- projectRaster(pr3, r)

\dontrun{

# using a higher resolution
res(pr) <- 10000
pr <- projectRaster(r, pr, method='bilinear')
inv <- projectRaster(pr, r, method='bilinear')
dif <- r - inv
plot(dif)

# Meuse data to lon/lat
meuse <- raster(system.file("external/test.grd", package="raster"))
meusell <- projectRaster(meuse, res=1/1200, crs="+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
}
}
}

\keyword{spatial}

