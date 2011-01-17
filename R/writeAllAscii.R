# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.writeAscii <- function(raster, filename, ...) {

	v <- getValues(raster)
	raster <- .startAsciiWriting(raster, filename, ...)
	if (raster@file@dtype == 'INT') {
		on.exit(options(scipen=options('scipen')))
		options(scipen=10)
		v <- round(v)
	}
	
	v[is.na(v)] <- raster@file@nodatavalue
	v <- matrix(v, ncol=ncol(raster), byrow=TRUE)
	write.table(v, raster@file@name, append = TRUE, quote = FALSE, sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)

	return( .stopAsciiWriting(raster) )
		
}
 
 