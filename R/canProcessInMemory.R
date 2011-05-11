# Authors: Robert J. Hijmans, r.hijmans@gmail.com 
# Date :  January 2009
# Version 0.9
# Licence GPL v3


canProcessInMemory <- function(raster, n=4) {

#	setOptions(overwrite=TRUE)
#	setOptions(format='GTiff')
#	return(FALSE)
	
	if (.toDisk()) { return(FALSE) } 

	n <- n + (nlayers(raster) - 1)
	cells <- round(1.1 * ncell(raster))

	if ((cells * n) > .maxmemory()) {
		return(FALSE) 
	}
	
	
#	if (substr( R.Version()$platform, 1, 7) == "i386-pc" ) {
#	# windows, function memory.size  available
#	memneed <- cells * 8 * n / (1024 * 1024)
#	memavail <- 0.5 * (memory.size(NA)-memory.size(FALSE))
#	if (memneed > memavail) {
#		return(FALSE)
#	} else {
#		return(TRUE)
#	}
#   } else {

	g <- gc()
	w <- getOption('warn')
	on.exit(options('warn'= w))
	options('warn'=-1) 
	r <- try( matrix(0.1, ncol=n, nrow=cells), silent=TRUE )

	if (class(r) == "try-error") {
		return( FALSE )
	} else {
		rm(r)
		g <- gc()
		return( TRUE ) 
	}
}

