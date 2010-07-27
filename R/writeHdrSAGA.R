# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  October 2009
# Version 0.9
# Licence GPL v3
 
.writeHdrSAGA <- function(raster) {
	hdrfile <- filename(raster)
	hdrfile <- .setFileExtensionHeader(hdrfile, 'SAGA')
	
	thefile <- file(hdrfile, "w")  # open an txt file connectionis
	cat("NAME\t=",  layerNames(raster), "\n", file = thefile)
	cat("DESCRIPTION\t= \n", file = thefile)
	cat("UNIT\t= \n", file = thefile)
	
	dtype <- .shortDataType(raster@file@datanotation)
	dsize <- dataSize(raster)
	# assuming SAGA supports BYTE, INTEGER (32), FLOAT
	if (dtype == 'INT' ) { 
		if (dsize == 1) {
			pixtype <- "BYTE"
		} else {
			pixtype <- "INTEGER"
		}
	} else { 
		pixtype <- "FLOAT" 
	}
	cat("DATAFORMAT\t=", pixtype, "\n", file = thefile)
	
	cat("DATAFILE_OFFSET\t= 0\n", file = thefile)
    if (.Platform$endian == 'little') { bo <- 'FALSE' } else { bo <- 'TRUE' }
	cat("BYTEORDER_BIG\t=", bo, "\n", file = thefile)

	cat("POSITION_XMIN\t= ",  as.character(xmin(raster) + 0.5 * xres(raster)), "\n", file = thefile)
	cat("POSITION_YMIN\t= ",  as.character(ymin(raster) + 0.5 * yres(raster)), "\n", file = thefile)

	cat("CELLCOUNT_Y\t= ",  nrow(raster), "\n", file = thefile)
	cat("CELLCOUNT_X\t= ",  ncol(raster), "\n", file = thefile)
	cat("CELLSIZE\t= ",  xres(raster), "\n", file = thefile)
	cat("Z_FACTOR\t= 1.000000\n", file = thefile)
    cat("NODATA_VALUE\t=", .nodatavalue(raster), "\n", file = thefile)	
    cat("TOPTOBOTTOM\t= TRUE", "\n", file = thefile)	
	close(thefile)
	
	return(invisible(TRUE))
}
