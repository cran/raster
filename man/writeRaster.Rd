\name{writeRaster}

\alias{writeRaster,RasterLayer,character-method}
\alias{writeRaster,RasterStack,character-method}
\alias{writeRaster,RasterBrick,character-method}

\alias{writeRaster}

\title{Write raster data to a file}

\description{
Write an entire Raster* object to a file, using one of the many supported formats. See \code{\link[raster]{writeValues}} for writing in chunks (e.g.  by row). 

When writing a file to disk, the file format is determined by the 'format=' argument if supplied, or else by the file extension (if the extension is known). If other cases the default format is used. The 'factory-fresh' default format is 'raster', but this can be changed using \code{\link[raster]{setOptions}}.
}

\usage{
writeRaster(x, filename, ...)
}

\arguments{
  \item{x}{Raster* object}
  \item{filename}{Output filename}
  \item{...}{Additional arguments. See below, under Methods}    
}

\section{Methods}{
\describe{
\bold{Full function call when \code{x} is a RasterLayer object}

\code{writeRaster(x, filename='', ...) }
 
\tabular{rll}{
\tab \code{x} \tab RasterLayer object \cr
\tab \code{filename} \tab Output filename \cr
\tab \code{...} \tab Additional arguments. See below \cr
}

\bold{Full function call when \code{x} is a RasterBrick or RasterStack object}

\code{writeRaster(x, filename='', bandorder='BIL', ...) }
 
\tabular{rll}{
\tab \code{x} \tab RasterLayer object \cr
\tab \code{filename} \tab Output filename \cr
\tab \code{bandorder} \tab Character. 'BIL', 'BIP', or 'BSQ' \cr
\tab \code{..} \tab Additional arguments. See below \cr
}

\bold{Additional arguments}
\tabular{rll}{
\tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeFormats}} \cr
\tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
\tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
}

}}

\details{
See \code{writeFormats} for supported file types ("formats", "drivers").

The rgdal package is needed for this most formats. Except for the 'raster', 'BIL', 'BIP', 'BSQ', 'SAGA', 'ascii', 'IDRISI', and 'CDF' formats. Some of these formats can be used with or without rgdal (idrisi, SAGA, ascii). 

In multi-band files  (i.e. files saved from RasterStack or RasterBrick objects), in the native 'raster' format, the bandorder can be set to BIL ('Bands Interleaved by Line'), BIP ('Bands Interleaved by Pixels') or BSQ ('Bands SeQuential'). Note that bandorder is not the same as filetype here. 

Supported file types include:

\tabular{llllr}{
	\tab \bold{File type} \tab \bold{Long name} \tab \bold{default extension} \tab \bold{Multiband support} \cr
    \tab \code{raster}    \tab 'Native' raster package format \tab .grd \tab Yes \cr
    \tab \code{ascii}     \tab ESRI Ascii  \tab .asc                 \tab  No \cr
    \tab \code{SAGA}      \tab SAGA GIS    \tab .sdat                \tab  No \cr
    \tab \code{IDRISI}    \tab IDRISI      \tab .rst                 \tab  No \cr
    \tab \code{CDF}       \tab netCDF (requires ncdf) \tab .nc    \tab  pending \cr
    \tab \code{GTiff}     \tab GeoTiff (requires rgdal) \tab .tif    \tab  Yes  \cr
    \tab \code{ENVI}      \tab ENVI .hdr Labelled       \tab .envi   \tab Yes  \cr
    \tab \code{EHdr}      \tab ESRI .hdr Labelled       \tab  .bil   \tab Yes  \cr
    \tab \code{HFA}       \tab Erdas Imagine Images (.img)  \tab .img \tab Yes  \cr
}
}

\value{
This function is used for the side-effect of writing values to a file.
}

\author{Robert J. Hijmans}

\seealso{\code{\link[raster]{writeFormats}}, and for faster writing: \code{\link[raster]{writeValues}} }  

\examples{ 
rst <- raster(system.file("external/test.grd", package="raster"))

# take a small part
rst <- crop(rst, extent(179880, 180800, 329880, 330840) )

# write to an integer binary file
r <- writeRaster(rst, filename="allint.grd", datatype='INT4S', overwrite=TRUE)

# write to a new geotiff file (depends on rgdal)
if (require(rgdal)) {
  r <- writeRaster(rst, filename="test.tif", format="GTiff", overwrite=TRUE)
}

# make a brick and save multi-band file
b <- brick(rst, sqrt(rst))
b <- writeRaster(b, filename="mutli.grd", bandorder='BIL', overwrite=TRUE)
 
# write to netcdf 
if (require(ncdf)) {	
    rnc <- writeRaster(rst, filename='netCDF.nc', format="CDF", overwrite=TRUE)   
}
}

\keyword{ spatial }
\keyword{ methods }

