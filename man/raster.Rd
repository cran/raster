\name{raster}

\docType{methods}

\alias{raster}
\alias{raster,missing-method}
\alias{raster,character-method}
\alias{raster,Extent-method}
\alias{raster,BasicRaster-method}
\alias{raster,RasterStack-method}
\alias{raster,RasterBrick-method}
\alias{raster,Spatial-method}
\alias{raster,SpatialGrid-method}
\alias{raster,SpatialPixels-method}
\alias{raster,matrix-method}
\alias{raster,list-method}
\alias{raster,asc-method}
\alias{raster,kasc-method}
\alias{raster,kde-method}
\alias{raster,grf-method}

\title{Create a RasterLayer object}

\description{
Methods to create a RasterLayer object. RasterLayer objects can be created from scratch, a filename, an Extent object, a matrix, an 'image' object, or from a Raster*, Spatial*, asc, kasc (adehabitat*), grf (geoR) or kde object.

In many cases, e.g. when a RasterLayer is created from a file, it does (initially) not contain any cell (pixel) values, it only has the parameters that describe the RasterLayer.

For an overview of the functions in the raster package have a look here: \code{\link{raster-package}}.
}


\arguments{
  \item{x}{Filename (character), Extent, Raster*, SpatialPixels*, SpatialGrid* object, matrix, or missing}
  \item{...}{Additional arguments, see below }  
}

\usage{
raster(x, ...)
}

\section{Methods}{
\describe{

\bold{1) Create a RasterLayer object from a file}

\code{raster(x, band=1, ...)}


\tabular{rll}{
\tab \code{x} \tab character. Name of raster file  \cr
\tab \code{band} \tab integer. Band number in case of a file of multiple bands, default = 1 \cr
\tab \code{...} \tab additional arguments.  \cr
}

Additional arguments:
\tabular{rll}{
\tab \code{native} \tab Logical.  Default is \code{FALSE} except when package rgdal is missing. If \code{TRUE}, reading and writing of IDRISI, BIL, BSQ, BIP, SAGA, and Arc ASCII files is done with native (raster package) drivers, rather then via rgdal. 'raster' and netcdf format files are always read with native drivers. 
}

For netCDF files (CF convention):
\tabular{rll}{
\tab \code{varname} \tab character. The variable name (e.g. 'altitude' or 'precipitation'. If not supplied and the file has multiple variables are a guess will be made (and reported))\cr
\tab \code{band} \tab integer > 0. The 'band' (layer) number of the file. E.g., the 'time' variable (if there are any) (default=NA)\cr
}
To read netCDF files, the \code{ncdf} package needs to be available.  


If \code{x} is a character value, it should be a filename of a file that the raster package can read. Supported file types are the 'native' raster package format and those that can be read via rgdal. See \code{\link[rgdal]{readGDAL}} help for supported file types.


\bold{2) Create a RasterLayer object from scratch}

\code{raster(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84")}

\tabular{rll}{
 \tab \code{nrows} \tab number of rows \cr
 \tab \code{ncols} \tab number of columns \cr
 \tab \code{xmn} \tab minimum x coordinate (left border) \cr
 \tab \code{xmx} \tab maximum x coordinate (right border) \cr
 \tab \code{ymn} \tab minimum y coordinate (bottom border) \cr
 \tab \code{ymx} \tab maximum y coordinate (top border) \cr
 \tab \code{crs} \tab Character or object of class CRS. PROJ4 type description of a Coordinate Reference System (map projection) \cr
}
(item \code{x} is 'missing' in this case)

 
\bold{3) Create a RasterLayer object from an Extent object}
  
\code{raster(x, nrows=10, ncols=10, crs=NA)}

\tabular{rll}{
 \tab \code{x} \tab Extent object \cr
 \tab \code{nrows} \tab number of rows  \cr
 \tab \code{ncols} \tab number of columns \cr
 \tab \code{crs} \tab PROJ4 type description of a map projection \cr
}


\bold{4) Create a RasterLayer object from a Raster* object.} 

This copies the parameters of a Raster* object to a new RasterLayer, 
but does not copy the filename nor the cell values associated with the original Raster* object. 

\code{raster(x)}

\tabular{rll}{
 \tab \code{x} \tab a Raster* object \cr
}


\bold{5) Create a RasterLayer object from a RasterStack or RasterBrick object.}

\code{raster(x, layer=0)}

\tabular{rll}{
 \tab \code{x} \tab a RasterStack, SpatialPixels* or SpatialGrid* object \cr
 \tab \code{layer} \tab Integer. The layer from which to copy values to the new RasterLayer, if \code{layer > 0}.  \cr
}


\bold{6) Create a RasterLayer object from a RasterStack, RasterBrick, SpatialPixels* or SpatialGrid* object.}

\code{raster(x, layer=0)}

\tabular{rll}{
 \tab \code{x} \tab RasterStack, RasterBrick, SpatialPixels* or SpatialGrid* object \cr
 \tab \code{layer} \tab Integer. the layer from which to copy values to the new RasterLayer, if \code{layer > 0}\cr
}


\bold{7) Create a RasterLayer object from a matrix.} 

The default extent is set to be between 0 and 1 in the x and y direction but can be changed at creation of the RasterLayer object or later. You can also provide a projection.  

\code{function(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA)}

\tabular{rll}{
 \tab \code{x} \tab matrix \cr
 \tab \code{xmn} \tab minimum x coordinate (left border) \cr
 \tab \code{xmx} \tab maximum x coordinate (right border) \cr
 \tab \code{ymn} \tab minimum y coordinate (bottom border) \cr
 \tab \code{ymx} \tab maximum y coordinate (top border) \cr
 \tab \code{crs} \tab PROJ4 type description of a map projection (optional) \cr
}


\bold{8) Create a RasterLayer object from a 'image' object (a list with vectors x and y and matrix z), or from an asc, kasc, or kde object }

\code{raster(img, crs)}

\tabular{rll}{
 \tab \code{x} \tab RasterStack, SpatialPixels* or SpatialGrid* object \cr
 \tab \code{crs} \tab PROJ4 type description of a map projection (optional) \cr
}
}}


\details{
New RasterLayer objects normally have no values in memory. 
If they are created from a file on disk, you can access values with \code{\link[raster]{getValues}, \link[raster]{extract}} and related functions.
You can assign new values with  \code{\link[raster]{setValues}} and with \code{\link[raster]{replacement}}.
}

\value{
RasterLayer object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{stack}, \link[raster]{brick}} }

\examples{
#from file
#   For your own files, omit the 'system.file' and 'package="raster"' bits
#   these are just to get the path to files installed with the package
f <- system.file("external/test.grd", package="raster")
f
r <- raster(f)
logo <- raster(system.file("external/rlogo.grd", package="raster")) 


#from scratch
r1 <- raster(nrows=108, ncols=21, xmn=0, xmx=10)

#from an Extent object
e <- extent(r)
r2 <- raster(e)

#from another Raster* object
r3 <- raster(r)
s <- stack(r, r)
r3 <- raster(s)
}

\keyword{methods}
\keyword{spatial}

