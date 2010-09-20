\name{writeRasterHdr}

\alias{writeRasterHdr}

\title{Header files}

\description{
Write header files to use together with raster binary files to read the data in other applications. 
}

\usage{
writeRasterHdr(raster, format) 
}

\arguments{
  \item{raster}{ RasterLayer object }
  \item{format}{ Type of header file: 'BIL', 'ENVI', 'ErdasRaw', 'IDRISI', 'raster', 'SAGA' }
}

\details{
The RasterLayer object must be associated with a file on disk.

You can use writeRaster to save a existing file in another format. But if you have a file in a 'raster' format (or similar), you can also only export a header file, 
and use the data file that already exists. writeHeader can write an ENVI, or BIL header (.hdr) file; an Erdas Raw (.raw) header file; an IDRISI (.rdc) or SAGA (.sgrd). 
This (hopefully) allows for reading the binary data ("raster.gri"), perhaps after changing the file extension, in other programs such as ENVI or ArcMap.
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{writeRaster}}, \code{\link[rgdal]{writeGDAL}} }

\examples{ 
r <- raster(system.file("external/test.grd", package="raster"))
r <- writeRaster(r, filename='export.grd')
writeRasterHdr(r, format="ENVI") 
}

\keyword{ spatial }
