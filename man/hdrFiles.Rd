\name{hdr}

\alias{hdr}

\title{Header files}

\description{
Write header files to use together with raster binary files to read the data in other applications. 
}

\usage{
hdr(x, format, extension='.wld') 
}

\arguments{
  \item{x}{RasterLayer or RasterBrick object associated with a binary values file on disk }
  \item{format}{Type of header file: 'VRT', 'BIL', 'ENVI', 'ErdasRaw', 'IDRISI', 'SAGA', 'RASTER', 'WORLDFILE', 'PRJ' }
  \item{extension}{File extension, only used with an ESRI worldfile (\code{format='WORLDFILE'})}
}

\details{
The RasterLayer object must be associated with a file on disk.

You can use \code{\link{writeRaster}} to save a existing file in another format. But if you have a file in a 'raster' format (or similar), you can also only export a header file, 
and use the data file (.gri) that already exists. writeHeader can write a VRT (GDAL virtual raster) header (.vrt); an ENVI or BIL header (.hdr) file; an Erdas Raw (.raw) header file; an IDRISI (.rdc) or SAGA (.sgrd). 
This (hopefully) allows for reading the binary data (.gri), perhaps after changing the file extension, in other programs such as ENVI or ArcGIS.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{writeRaster}}, \code{\link[rgdal]{writeGDAL}} }

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
r <- writeRaster(r, filename='export.grd')
hdr(r, format="ENVI") 
}

\keyword{ spatial }
