# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3


.writeAscii <- function(x, filename, ...) {

	v <- getValues(x)
	x <- .startAsciiWriting(x, filename, ...)
	if (x@file@dtype == 'INT') {
		on.exit(options(scipen=options('scipen')))
		options(scipen=10)
		v <- round(v)
	}
	
	v[is.na(v)] <- x@file@nodatavalue
	v <- matrix(v, ncol=ncol(x), byrow=TRUE)
	write.table(v, x@file@name, append = TRUE, quote = FALSE, sep = " ", eol = "\n", dec = ".", row.names = FALSE, col.names = FALSE)

	return( .stopAsciiWriting(x) )
		
}
 
 