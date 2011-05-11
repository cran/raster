# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  April 2011
# Version 1.0
# Licence GPL v3


.writeHdrPRJ <- function(x, ESRI=TRUE) {

	if ( ! .requireRgdal() ) { 
		stop('you need to install the rgdal package to be able use this function')
	}

	p4s <- try(	showWKT(projection(x), file = NULL, morphToESRI = ESRI) )
	if (class(p4s) != 'try-error') {
		prjfile <- filename(x)
		ext(prjfile) <- '.prj'
		cat(p4s, file=filename)
	} else {
		return(FALSE)
	}
	return(invisible(TRUE))
}

	
