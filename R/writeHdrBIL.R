# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2009
# Version 0.9
# Licence GPL v3

 
.writeHdrBIL <- function(raster, layout='BIL') {
	hdrfile <- raster@file@name
	ext(hdrfile) <- '.hdr'
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("NROWS          ",  nrow(raster), "\n", file = thefile)
	cat("NCOLS          ",  ncol(raster), "\n", file = thefile)
	cat("NBANDS         ",  nbands(raster), "\n", file = thefile)
	cat("NBITS          ",  dataSize(raster@file@datanotation) * 8, "\n", file = thefile)
	if (.Platform$endian == "little") { 
		btorder <- "I" 
	} else { btorder <- "M" 
	}
	cat("BYTEORDER      ", btorder, "\n", file = thefile)
	
#  PIXELTYPE should work for Gdal, and perhpas ArcGIS, see:
# http://lists.osgeo.org/pipermail/gdal-dev/2006-October/010416.html	

	dtype <- .shortDataType(raster@file@datanotation)
	if (dtype == 'INT' | dtype == 'LOG' ) { 
		if (dataSigned(raster@file@datanotation)) {
			pixtype <- "SIGNEDINT"
		} else {
			pixtype <- "INT"
		}
	} else { 
		pixtype <- "FLOAT" 
	}
	cat("PIXELTYPE      ", pixtype, "\n", file = thefile)	
	cat("LAYOUT         ", layout, "\n", file = thefile)
    cat("SKIPBYTES       0\n", file = thefile)
    cat("ULXMAP         ", as.character(xmin(raster) + 0.5 * xres(raster)), "\n", file = thefile) 
    cat("ULYMAP         ", as.character(ymax(raster) - 0.5 * yres(raster)), "\n", file = thefile) 
	cat("XDIM           ", xres(raster), "\n", file = thefile)
	cat("YDIM           ", yres(raster), "\n", file = thefile)
	browbytes <- round(ncol(raster) * dataSize(raster@file@datanotation) )
	cat("BANDROWBYTES   ", browbytes, "\n", file = thefile)
	cat("TOTALROWBYTES  ", browbytes *  nbands(raster), "\n", file = thefile)
	cat("BANDGAPBYTES    0\n", file = thefile)
    cat("NODATA         ", .nodatavalue(raster), "\n", file = thefile)	

	cat("\n\n", file = thefile)
	cat("The below is additional metadata, not part of the BIL/HDR format\n", file = thefile)
	cat("----------------------------------------------------------------\n", file = thefile)
	cat("CREATOR=R package:raster\n", file = thefile)
	cat("CREATED=", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n", file = thefile)
	cat("Projection=", projection(raster), "\n", file = thefile)
	cat("MinValue=",  minValue(raster), "\n", file = thefile)
	cat("MaxValue=",  maxValue(raster), "\n", file = thefile)

	close(thefile)
	return(invisible(TRUE))	
}
