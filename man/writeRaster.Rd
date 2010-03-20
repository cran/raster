\name{writeRaster}

\alias{writeRaster,RasterLayer,character-method}
\alias{writeRaster,RasterStack,character-method}
\alias{writeRaster,RasterBrick,character-method}

\alias{writeRaster}

\title{Write raster data to a file}

\description{
Write raster values to a file. Either as binary ('raster') format, Ascii (ESRI Arc Ascii) format, and other GDAL supported formats. 
You can write an entire raster dataset at once, or by row.
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
Values in a \code{RasterLayer} object are written to a file. Relatively small datasets can be loaded into memory, manipulated, and saved to disk in their entirety. Large datasets can be read and written row by row. 
When writing by row, you must write all rows, and you must write them in sequence. Start at row=1, and end at row=nrow(raster). Only when all rows have been appended to the data file on disk, the file becomes readable. You cannot overwrite an isolated single row in an existing raster file. 

In multi-band files (i.e. files saved from RasterStack or RasterBrick objects), the bandorder can be set to BIL ('Bands Interleaved by Line'), BIP ('Bands Interleaved by Pixels') or BSQ ('Bands SeQuential')

Note that the values of a RasterBrick and RasterLayer objects can be written to a single file in a row by row fashion; but the values of a RasterStack can only be saved to a file in one go.  

See \code{writeFormats} for supported file types ("formats", "drivers"); the rgdal package is needed for this function, exxcept for the 'raster', ascii, and 'CDF' formats.

Supported file types include:

\tabular{lllr}{
	\tab \bold{File type} \tab \bold{Long name} \tab \bold{Multiband support} \cr
    \tab \code{raster} \tab 'Native' raster package format \tab Yes \cr
    \tab \code{ascii}  \tab ESRI Ascii \tab No \cr
    \tab \code{SAGA}  \tab SAGA GIS \tab No \cr
    \tab \code{IDRISI}  \tab IDRISI \tab No \cr
    \tab \code{CDF}  \tab netCDF (requires RNetCDF) \tab pending \cr
    \tab \code{GTiff}  \tab GeoTiff (requires rgdal) \tab Yes  \cr
    \tab \code{ENVI}   \tab ENVI .hdr Labelled \tab Yes  \cr
    \tab \code{EHdr}   \tab ESRI .hdr Labelled \tab Yes  \cr
    \tab \code{HFA}  \tab Erdas Imagine Images (.img) \tab Yes  \cr
}
}

\value{
This function is used for the side-effect of writing values to a file.
}

\author{Robert J. Hijmans}

\seealso{\code{\link[raster]{writeFormats}} }  

\examples{ 
rst <- raster(system.file("external/test.grd", package="raster"))
 
# read all data
rst <- readAll(rst)

# write all to a new binary file
rst <- writeRaster(rst, filename="binall.grd", overwrite=TRUE)

# write all to a new ERDAS .img file
rst <- writeRaster(rst, filename="binall.img", format="HFA", overwrite=TRUE)

# write all to an integer binary file
rst <- writeRaster(rst, filename="binallint.grd", datatype='INT4S', overwrite=TRUE)
 
# write all to ascii file
rst <- writeRaster(rst, filename="ascall.asc", format='ascii', overwrite=TRUE)

# make a brick and save multi-band file
b <- brick(rst, sqrt(rst))
rst <- writeRaster(b, filename="mutli.grd", bandorder='BIL', overwrite=TRUE)
 
# read and write row by row; write to ascii file
rst <- raster(system.file("external/test.grd", package="raster"))
ascras <- raster(rst)
for (r in 1:nrow(rst)) {
	rst <- readRow(rst, r)
	v <- values(rst) * 10
	ascras <- setValues(ascras, v, r)
	ascras <- writeRaster(ascras, filename='ascrow.asc', format='ascii', overwrite=TRUE) 
}
	
# read and write row by row; write to binary file
binras <- raster(rst)
for (r in 1:nrow(rst)) {
	rst <- readRow(rst, r)
	binras <- setValues(binras, values(rst), r)
	binras <- writeRaster(binras, 'binbyrow', overwrite=TRUE) 
}

# read and write row by row; write to GeoTiff binary file
binras <- raster(rst)
for (r in 1:nrow(rst)) {
	rst <- readRow(rst, r)
	binras <- setValues(binras, values(rst), r)
	binras <- writeRaster(binras, 'binbyrow2.tif', format="GTiff", overwrite=TRUE) 
}

# write to netcdf 
if (require(RNetCDF)) {	
    rst <- readAll(rst)
    writeRaster(rst, filename='netCDF.nc', format="CDF", overwrite=TRUE)   
}

}

\keyword{ spatial }
\keyword{ methods }
