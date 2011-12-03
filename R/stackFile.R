# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3



stackOpen <- function(stackfile) {
	st <- read.table(stackfile, as.is=FALSE, strip.white=TRUE)
	if (dim(st)[2] > 1) {
		rst <- stack(as.vector(st[,1]), bands=as.vector(st[,2]))
	} else {
		rst <- stack(as.vector(st[,1]))
	}
	rst@filename <- stackfile
	return(rst)
}

stackSave <- function(x, filename) {
	filename <- trim(filename)
	if (filename == "") { stop('Provide a non empty filename.') }
	thefile <- file(filename, "w")
	for (i in 1:length(x@layers)) {
		fname <- trim(filename(x@layers[[i]]))
		if (fname == "") {
			stop("cannot save a RasterStack that has layers that only exist in memory. Use writeStack first/instead.")
		}	
		cat(fname, "\t", bandnr(x@layers[[i]]),"\n", file=thefile)
	}
	close(thefile)
	x@filename <- filename
	return(x)
}

