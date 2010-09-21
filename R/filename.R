# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

filename <- function(x) {
	if (inherits(x, 'RasterStack')) { 
		return(x@filename) 
	} 
	return(x@file@name)
}


'.xfilename<-' <- function(x, value) {
	filename <- trim(value)
	if (is.na(filename) | is.null(filename) | !is.character(value)) {
		filename <- ""
	}
	if (filename != '') {
		filename <- path.expand(filename)
		p <- dirname(filename)
		if (p == '.') {  #current directory
			filename <- paste(getwd(), '/', filename, sep='')
		}
	}
	
# could also throw in normalizePath(utils) 
	if (inherits(x, 'RasterStack')) {
		ext(filename) <- ".stk"
		x@filename <- filename
	} else {
		x@file@name <- filename
		if (inherits(x, 'RasterLayer')) {	
			shortname <- basename(filename)
			ext(shortname) <- ""
			shortname <- gsub(" ", "_", shortname)
			if (nbands(x) > 1) { 
				shortname <- paste(shortname, "_", band(x), sep="") 
			} 
			x@layernames <- shortname
		}
	}
	return(x)	
}

