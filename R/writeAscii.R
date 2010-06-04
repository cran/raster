# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.writeAscii <- function(raster, filename, ...) {
 	filename <- trim(filename)
	if (filename == '') {
		stop('provide a filename')
	}
	raster@file@name <- filename
	overwrite <- .overwrite(...)
	dtype  <- .shortDataType(.datatype(...))
	
	if (dataIndices(raster)[1] == 1) {
		resdif <- abs((yres(raster) - xres(raster)) / yres(raster) )
		if (resdif > 0.01) {
			stop(paste("raster has unequal horizontal and vertical resolutions. Such data cannot be stored in arc-ascii format"))
		} else if (resdif > 0.001) {
			warning("arc-ascii format ignore that this raster has slightly unequal horizontal and vertical resolutions")
		}
		if (!overwrite & file.exists(filename)) {
				stop(paste(filename, "exists. Use 'overwrite=TRUE'")) 
		}

		thefile <- file(filename, "w")  # open an txt file connection
		cat("NCOLS", ncol(raster), "\n", file = thefile)
		cat("NROWS", nrow(raster), "\n", file = thefile)
		cat("XLLCORNER", xmin(raster), "\n", file = thefile)
		cat("YLLCORNER", ymin(raster), "\n", file = thefile)
		cat("CELLSIZE",  xres(raster), "\n", file = thefile)
		cat("NODATA_value", .nodatavalue(raster), "\n", file = thefile)
		close(thefile) #close connection
		
    } else if ( dataIndices(raster)[2] > ncell(raster)) {
		stop(paste('writing beyond end of file. last cell:', dataIndices(raster)[2], '>', ncell(raster)))
	}

	if (dtype == 'INT') {
		raster@data@values <- round(raster@data@values)
	}
	raster@data@values[is.na(raster@data@values)] <- .nodatavalue(raster)
	
	
	if (dataContent(raster) == 'all') {
		write.table(getValues(raster, format='matrix'), filename, append = TRUE, quote = FALSE, 
								sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
								
		return(raster(filename))
		
	} else {
		write.table(t(raster@data@values), filename, append = TRUE, quote = FALSE, 
							sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)
    	
		if ( dataIndices(raster)[2] == ncell(raster)) {
			return(raster(filename))
		} else {
			return(raster)
		}	
	}
}
 
 