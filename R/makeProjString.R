# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3



.newCRS <- function(projs) {
	if (is.null(projs) | is.na(projs) | nchar(projs) < 3) { 
		prj <- (CRS(as.character(NA)))
	} else {
		projs <- trim(projs)
		prj <- try(CRS(projs), silent = T)
		if (class(prj) == "try-error") { 
			warning(paste(projs, 'is not a valid proj4 CRS string')) 
			prj <- CRS(as.character(NA))
		}
	}
	return(prj)
}



.makeProj <- function(projection='longlat', ..., ellipsoid="", datum="", asText=TRUE) {
	prj <- projInfo("proj")
	ell <- projInfo("ellps")
	dat <- projInfo("datum")
	projection <- trim(projection)
	ellipsoid <- trim(ellipsoid)
	datum <- trim(datum)
	if (!(projection %in% prj[,1])) {
		stop("unknown projection. See projInfo()") 
	} else {
		pstr <- paste('+proj=',projection, sep="")
		projname <- as.vector(prj[which(prj[,1]==projection), 2])
	}
	pargs <- list(...)
	if ( length(pargs) > 0 ) {
		for (i in 1:length(pargs)) {
			pstr <- paste(pstr, ' +', pargs[[i]], sep="")
		}
	}
	if (ellipsoid != "") {
		if (!(ellipsoid %in% ell[,1])) { 
			stop("unknown ellipsoid. See projInfo('ellps')") 
		} else {
			pstr <- paste(pstr, " +ellps=", ellipsoid, sep="")
#			ellipname <- ell[which(ell[,1]==ellipsoid), 2]
		}
	}
	if (datum != "") {
		if (!(datum %in% dat[,1])) { 
			stop("unknown datum. See projInfo('datum')") 
		} else {
			pstr <- paste(pstr, " +datum=", datum, sep="")
#			datumname <- as.vector(dat[which(dat[,1]==datum), 2])
		}
	}
	cat("Projection: ", projname[1], "\n")
	crs <- .newCRS(pstr)
	if (asText) { 
		return(trim(crs@projargs))
	} else {
		return(crs)
	}
}

