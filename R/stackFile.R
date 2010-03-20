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
	filename(rst) <- stackfile
	return(rst)
}

stackSave <- function(rstack) {
	stackfile <- trim(rstack@filename)
	if (stackfile == "") { stop('RasterStack does not have a filename.') }
	thefile <- file(stackfile, "w")
	for (i in 1:length(rstack@layers)) {
		fname <- trim(filename(rstack@layers[[i]]))
		if (trim(fname) == "") {
			stop("cannot save a RasterStack that has Layers without filenames. Use writeStack instead.")
		}	
		cat(fname, "\t", band(rstack@layers[[i]]),"\n", file=thefile)
	}
	close(thefile)
	return(rstack)
}

