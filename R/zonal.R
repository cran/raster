# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : March 2009
# Version 0.9
# Licence GPL v3

zonal <- function(x, zones, stat='mean', digits=0, na.rm=TRUE, progress='') {

	compare(c(x, zones))
	stopifnot(hasValues(zones))
	stopifnot(hasValues(x))
	
	layernames <- names(x)
	
	if (canProcessInMemory(x, 3)) {
		inmem <- TRUE
	} else {
		inmem <- FALSE
	}
	
	if (inmem) {
		pb <- pbCreate(2, progress)		
		fun <- match.fun(stat)
		x <- getValues(x)
		x <- cbind(x, round(getValues(zones), digits=digits))
		pb <- pbStep(pb, 1)		
		alltab <- aggregate(x[,1:(ncol(x)-1)], by=list(x[,ncol(x)]), FUN=fun, na.rm=na.rm) 
		stat <- deparse(substitute(stat))
		pb <- pbStep(pb, 2)
			
	} else {
		
		if (class(stat) != 'character') {
			stop("RasterLayers cannot be processed in memory.\n You can use stat='sum', 'mean', 'min', or 'max', but not a function")
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
		pb <- pbCreate(tr$n, progress)		
		
		for (i in 1:tr$n) {
			d <- getValuesBlock(x, row=tr$row[i], nrows=tr$nrows[i])
			d <- cbind(d,  round(getValuesBlock(zones, row=tr$row[i], nrows=tr$nrows[i]), digits=digits))
			if (nrow(d) > 0) {
				alltab <- rbind(alltab, aggregate(d[,1:(ncol(d)-1)], by=list(d[,ncol(d)]), FUN=fun, na.rm=na.rm)) 
				if (counts) {
					if (na.rm) {
						cnttab <- rbind(cnttab, aggregate(d[,1:(ncol(d)-1)], by=list(d[,ncol(d)]), FUN=function(x)length(na.omit(x))))
					} else {
						cnttab <- rbind(cnttab, aggregate(d[,1:(ncol(d)-1)], by=list(d[,ncol(d)]), FUN=length))				
					}
				}
				if (length(alltab) > 10000) {
					alltab <- aggregate(alltab[,2:ncol(alltab)], by=list(alltab[,1]), FUN=fun, na.rm=na.rm) 
					if (counts) {
						cnttab <- aggregate(cnttab[,2:ncol(cnttab)], by=list(cnttab[,1]), FUN=sum, na.rm=na.rm) 
					}
				}
			}
			pbStep(pb, i)
		}
			
		alltab <- aggregate(alltab[,2:ncol(alltab)], by=list(alltab[,1]), FUN=fun, na.rm=na.rm) 	
		if (counts) {
			cnttab <- aggregate(cnttab[,2:ncol(cnttab)], by=list(cnttab[,1]), FUN=sum) 
			alltab[2:ncol(alltab)] <- alltab[2:ncol(alltab)] / cnttab[2:ncol(alltab)]
		}
	}
	
	alltab = as.matrix(alltab)
	colnames(alltab)[1] <- 'zone'
	if (ncol(alltab) > 2) {
		colnames(alltab)[2:ncol(alltab)] <- layernames
	} else {
		colnames(alltab)[2] <- stat[1]
	}
	pbClose(pb)

	return(alltab)
}
