\name{brick}

\docType{methods}

\alias{brick}

\alias{brick,character-method}
\alias{brick,RasterLayer-method}
\alias{brick,RasterStack-method}
\alias{brick,RasterBrick-method}
\alias{brick,Extent-method}
\alias{brick,list-method}
\alias{brick,missing-method}
\alias{brick,SpatialPixels-method}
\alias{brick,SpatialGrid-method}
\alias{brick,array-method}

\title{ Create a RasterBrick object}

\description{
A RasterBrick is a multi-layer raster object. They are typically created from a multi-band file; but they can also exist entirely in memory. They are similar to a RasterStack, but processing time should be shorter when using a RasterBrick. Yet they are less flexible as they can only point to a single file.

A RasterBrick can be created from RasterLayer objects, from a RasterStack, or from a (multi-band) file. The can also be created from SpatialPixels*, SpatialGrid*, and Extent objects, and from an three-dimensional array.
}

\usage{brick(x, ...)}

\arguments{
 \item{x}{ Filename (character), Raster* object, array, SpatialGrid*, SpatialPixels*, Extent, or list of Raster* objects }
 \item{...}{ Additional arguments. See under Details}
}


\section{Methods}{
\describe{

\bold{1) Create a RasterBrick from a file}

\code{brick(x, ...)}

\tabular{rll}{
\tab \code{x} \tab character. A filename of a file that the raster package can read. Supported file types are the 'native' raster package format and those that can be read via rgdal. See \code{\link[rgdal]{readGDAL}} help for supported file types. \cr
\tab \code{...} \tab additional arguments.  \cr
}

Additional arguments:
\tabular{rll}{
\tab \code{native} \tab Logical.  Default is \code{FALSE} except when package rgdal is missing. If \code{TRUE}, reading and writing of IDRISI, BIL, BSQ, BIP, and ArcAscii files is done with native (raster package) drivers, rather then via rgdal. 'raster', RSAGA, and netcdf format files are always read with native drivers.
}

For netCDF files (CF convention):
\tabular{rll}{
\tab \code{varname} \tab character. The variable name (e.g. 'altitude' or 'precipitation'. If not supplied and the file has multiple variables are a guess will be made (and reported))\cr
\tab \code{band} \tab integer > 0. The 'band' (layer) number of the file. E.g., the 'time' variable (if there are any) (default=NA)\cr
}
To read netCDF files, the \code{ncdf} package needs to be available. 


\bold{2) Create a RasterBrick from scratch}

\code{brick(nrows=180, ncols=360, xmn=-180, xmx=180, ymn=-90, ymx=90, crs="+proj=longlat +datum=WGS84")}

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

 
\bold{3) Create a RasterBrick from an Extent object}
  
\code{brick(x, nrows=10, ncols=10, crs=NA)}

\tabular{rll}{
 \tab \code{x} \tab \link{Extent} object \cr
 \tab \code{nrows} \tab number of rows  \cr
 \tab \code{ncols} \tab number of columns \cr
  \tab \code{crs} \tab Character or object of class CRS. PROJ4 type description of a Coordinate Reference System (map projection) \cr
}


\bold{4) Create a RasterBrick from a RasterStack}

\code{brick(x, values=TRUE)}

\tabular{rll}{
 \tab \code{x} \tab a RasterStack object \cr
 \tab \code{values} \tab Logical. If \code{TRUE}, the values of the RasterStack are transferred to the RasterBrick \cr
}


\bold{5) Create a RasterBrick from a RasterLayer or RasterBrick} 

\code{brick(x)}

This copies the parameters of a RasterStack object to a new RasterBrick. The values of the RasterStack are transferred to the RasterBrick, unless \code{values=FALSE}.  

\tabular{rll}{
 \tab \code{x} \tab a Raster* object \cr
}


\bold{6) Create a RasterBrick from a SpatialPixels* or SpatialGrid*}

\code{brick(x, layer=0)}


\bold{7) Create a RasterBrick from an array } 

The default extent is set to be between 0 and 1 in the x and y direction but can be changed at creation of the RasterLayer object or later. You can also provide a projection.  

\code{function(x, xmn=0, xmx=1, ymn=0, ymx=1, crs=NA, transpose=FALSE)}

\tabular{rll}{
 \tab \code{x} \tab three-dimensional array (rows, columns, layers) \cr
 \tab \code{xmn} \tab minimum x coordinate (left border) \cr
 \tab \code{xmx} \tab maximum x coordinate (right border) \cr
 \tab \code{ymn} \tab minimum y coordinate (bottom border) \cr
 \tab \code{ymx} \tab maximum y coordinate (top border) \cr
 \tab \code{crs} \tab PROJ4 type description of a map projection (optional) \cr
 \tab \code{transpose} \tab Logical. Transpose the data? \cr
}

}}



\value{
RasterBrick object
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{raster}} }

\examples{
b <- brick(system.file("external/rlogo.grd", package="raster"))
b
nlayers(b)
layerNames(b)
extract(b, 870)
}

\keyword{methods}
\keyword{spatial}
