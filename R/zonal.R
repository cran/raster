# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

zonal <- function(x, zones, stat='mean', digits=0, progress) {

	compare(c(x, zones))
	
	layernames <- layerNames(x)
	
	if (missing(progress)) { progress <- .progress() }

	if (inMemory(x) & inMemory(zones)) {
		inmem <- TRUE
	} else if (canProcessInMemory(x, 3)) {
		inmem <- TRUE
	} else {
		inmem <- FALSE
	}
	
	if (inmem) {

		fun <- match.fun(stat)
		d <- getValues(x)
		rm(x)
		d <- cbind(d, round(getValues(zones), digits=digits))
		rm(zones)
		alltab <- aggregate(d[,1:(ncol(d)-1)], by=list(d[,ncol(d)]), FUN=fun) 
		stat <- deparse(substitute(stat))
			
	} else {
		
		if (class(stat) != 'character') {
			stop("RasterLayers are too large.\n You can use stat='sum', 'mean', 'min', or 'max', but not a function")
		}
		if (! stat %in% c('sum', 'mean', 'min', 'max')) {
			stop("stat can be 'sum', 'mean', 'min', or 'max'")
		}
		
		fun <- match.fun(stat)
		if (stat == 'mean') {
			fun <- sum
			counts <- TRUE
		} else {
			counts <- FALSE		
		}

		alltab <- array(dim=0)
		cnttab <- alltab
	
		tr <- blockSize(x, n=2)
		pb <- pbCreate(tr$n, type=.progress())			
		
		for (i in 1:tr$n) {
			d <- getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i])
			d <- cbind(d,  round(getValuesBlock(zones, row=tr$row[i], nrows=tr$nrows[i]), digits=digits))
			
			alltab <- rbind(alltab, aggregate(d[,1:(ncol(d)-1)], by=list(d[,ncol(d)]), FUN=fun)) 
			if (counts) {
				cnttab <- rbind(cnttab, aggregate(d[,1:(ncol(d)-1)], by=list(d[,ncol(d)]), FUN=length)) 
			}
			if (length(alltab) > 10000) {
				alltab <- aggregate(alltab[,2:ncol(alltab)], by=list(alltab[,1]), FUN=fun) 
				if (counts) {
					cnttab <- aggregate(cnttab[,2:ncol(cnttab)], by=list(cnttab[,1]), FUN=sum) 
				}
			}
			pbStep(pb, i)
		}
		pbClose(pb)
			
		alltab <- aggregate(alltab[,2:ncol(alltab)], by=list(alltab[,1]), FUN=fun) 	
		if (counts) {
			cnttab <- aggregate(cnttab[,2:ncol(cnttab)], by=list(cnttab[,1]), FUN=sum) 
			alltab[2:ncol(alltab)] <- alltab[2:ncol(alltab)] / cnttab[2:ncol(alltab)]
		}
	}
	
	colnames(alltab)[1] <- 'zone'
	if (ncol(alltab) > 2) {
		colnames(alltab)[2:ncol(alltab)] <- layernames
	} else {
		colnames(alltab)[2] <- stat
	}
	
	return(alltab)
}


