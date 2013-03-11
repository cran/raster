# Author: Robert J. Hijmans
# Date : March 2009
# Version 0.9
# Licence GPL v3


if (!isGeneric("zonal")) {
	setGeneric("zonal", function(x, z, ...)
		standardGeneric("zonal"))
}	



setMethod('zonal', signature(x='RasterLayer', z='RasterLayer'), 
	function(x, z, stat='mean', digits=0, na.rm=TRUE, ...) {

		compareRaster(c(x, z))
		stopifnot(hasValues(z))
		stopifnot(hasValues(x))
	
		layernames <- names(x)
	
		if (canProcessInMemory(x, 3)) {
			inmem <- TRUE
		} else {
			inmem <- FALSE
		}
	
		if (inmem) {
			pb <- pbCreate(2, label='zonal', ...)		
			fun <- match.fun(stat)
			x <- getValues(x)
			z <- round(getValues(z), digits=digits)
			pb <- pbStep(pb, 1)		
			alltab <- tapply(x, z, FUN=fun, na.rm=na.rm) 
			alltab <- cbind(as.numeric(names(alltab)), alltab)

			stat <- deparse(substitute(stat))
			pb <- pbStep(pb, 2)
			
		} else {
		
			if (class(stat) != 'character') {
				stop("RasterLayers cannot be processed in memory.\n You can use stat='sum', 'mean', 'sd', 'min', or 'max', but not a function")
			}
			if (! stat %in% c('sum', 'mean', 'sd', 'min', 'max')) {
				stop("stat can be 'sum', 'mean', 'sd', 'min', or 'max'")
			}
			sdtab <- FALSE
			fun <- match.fun(stat)
			if ( stat == 'mean' | stat == 'sd') {
				fun <- sum
				counts <- TRUE
				if (stat == 'sd') {
					sdtab <- TRUE
				}
			} else {
				counts <- FALSE		
			}

			alltab <- array(dim=0)
			sqtab <- cnttab <- alltab
	
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, label='zonal', ...)
		
			nc <- nlayers(x)
			nc1 <- nc + 1
			nc2 <- 2:nc1
			
			for (i in 1:tr$n) {
				d <- cbind(getValues(x, row=tr$row[i], nrows=tr$nrows[i]))
				Z <- round(getValues(z, row=tr$row[i], nrows=tr$nrows[i]), digits=digits)
				#cat(i, '\n')
				#flush.console()
				
				a <- tapply(d, Z, FUN=fun, na.rm=na.rm)
				a <- cbind(as.numeric(names(a)), a)
				alltab <- rbind(alltab, a) 
				if (counts) {
					if (na.rm) {
						a <- tapply(d, Z, FUN=function(x)length(na.omit(x)))
						a <- cbind(as.numeric(names(a)), a)
						cnttab <- rbind(cnttab, a)
						if (sdtab) {
							a <- tapply( d^2, Z, FUN=function(x)sum(na.omit(x)))
							a <- cbind(as.numeric(names(a)), a)
							sqtab <- rbind(sqtab, a)
						}
					} else {
						a <- tapply(d, Z, FUN=length)
						a <- cbind(as.numeric(names(a)), a)
						cnttab <- rbind(cnttab, a)
						if (sdtab) {
							a <- tapply(d^2, Z, FUN=sum)
							a <- cbind(as.numeric(names(a)), a)
							sqtab <- rbind(sqtab, a)
						}
					}
				}
				if (length(alltab) > 10000) {
					alltab <- tapply(alltab[,nc2], alltab[,1], FUN=fun, na.rm=na.rm) 
					alltab <- cbind(as.numeric(names(alltab)), alltab)
					if (counts) {
						cnttab <- tapply(cnttab[,nc2], cnttab[,1], FUN=sum, na.rm=na.rm) 
						cnttab <- cbind(as.numeric(names(cnttab)), cnttab)
						if (sdtab) {
							sqtab <- tapply(sqtab[,nc2], sqtab[,1], FUN=sum, na.rm=na.rm) 
							sqtab <- cbind(as.numeric(names(sqtab)), sqtab)
						}
					}
				}
				pbStep(pb, i)
			}
			
			alltab <- tapply(alltab[,nc2], alltab[,1], FUN=fun, na.rm=na.rm)
			alltab <- cbind(as.numeric(names(alltab)), alltab)
			if (counts) {
				cnttab <- tapply(cnttab[,nc2], cnttab[,1], FUN=sum) 
				cnttab <- cbind(as.numeric(names(cnttab)), cnttab)
				alltab[nc2] <- alltab[nc2] / cnttab[nc2]
				if (sdtab) {
					sqtab <- tapply(sqtab[,nc2], sqtab[,1], FUN=sum, na.rm=na.rm) 
					sqtab <- cbind(as.numeric(names(sqtab)), sqtab)
					alltab[nc2] <- sqrt(( (sqtab[,nc2] / cnttab[,nc2]) - (alltab[nc2])^2 ) * (cnttab[,nc2]/(cnttab[,nc2]-1)))
				}
				
			}
		}
	
		#alltab <- as.matrix(alltab)
		colnames(alltab)[1] <- 'zone'
		colnames(alltab)[2] <- stat[1]
		pbClose(pb)
	
		return(alltab)
	}
)

#zonal(r, z, 'sd')




setMethod('zonal', signature(x='RasterStackBrick', z='RasterLayer'), 
	function(x, z, stat='mean', digits=0, na.rm=TRUE, ...) {

		compareRaster(c(x, z))
		stopifnot(hasValues(z))
		stopifnot(hasValues(x))
	
		layernames <- names(x)
	
		if (canProcessInMemory(x, 3)) {
			inmem <- TRUE
		} else {
			inmem <- FALSE
		}
	
		if (inmem) {
			pb <- pbCreate(2, label='zonal', ...)		
			fun <- match.fun(stat)
			x <- getValues(x)
			x <- cbind(x, round(getValues(z), digits=digits))
			pb <- pbStep(pb, 1)		
			alltab <- aggregate(x[,1:(ncol(x)-1)], by=list(x[,ncol(x)]), FUN=fun, na.rm=na.rm) 
			stat <- deparse(substitute(stat))
			pb <- pbStep(pb, 2)
			
		} else {
		
			if (class(stat) != 'character') {
				stop("RasterLayers cannot be processed in memory.\n You can use stat='sum', 'mean', 'sd', 'min', or 'max', but not a function")
			}
			if (! stat %in% c('sum', 'mean', 'sd', 'min', 'max')) {
				stop("stat can be 'sum', 'mean', 'sd', 'min', or 'max'")
			}
			sdtab <- FALSE
			fun <- match.fun(stat)
			if ( stat == 'mean' | stat == 'sd') {
				fun <- sum
				counts <- TRUE
				if (stat == 'sd') {
					sdtab <- TRUE
				}
			} else {
				counts <- FALSE		
			}

			alltab <- array(dim=0)
			sqtab <- cnttab <- alltab
	
			tr <- blockSize(x, n=2)
			pb <- pbCreate(tr$n, label='zonal', ...)
		
			nc <- nlayers(x)
			nc1 <- nc + 1
			nc2 <- 2:nc1
			
			# it might be more efficient to loop over the layers, particularly for a RasterStack
			
			for (i in 1:tr$n) {
				d <- cbind(getValues(x, row=tr$row[i], nrows=tr$nrows[i]),   
					 round(getValues(z, row=tr$row[i], nrows=tr$nrows[i]), digits=digits))
				#cat(i, '\n')
				#flush.console()
				alltab <- rbind(alltab, aggregate(d[,1:nc], by=list(d[,nc1]), FUN=fun, na.rm=na.rm)) 
				if (counts) {
					if (na.rm) {
						cnttab <- rbind(cnttab, aggregate(d[,1:nc], by=list(d[,nc1]), FUN=function(x)length(na.omit(x))))
						if (sdtab) {
							sqtab <- rbind(sqtab, aggregate( (d[,1:nc])^2, by=list(d[,nc1]), FUN=function(x)sum(na.omit(x))))
						}
					} else {
						cnttab <- rbind(cnttab, aggregate(d[,1:nc], by=list(d[,nc1]), FUN=length))				
						if (sdtab) {
							sqtab <- rbind(sqtab, aggregate( (d[,1:nc])^2, by=list(d[,nc]), FUN=sum))
						}
					}
				}
				if (length(alltab) > 10000) {
					alltab <- aggregate(alltab[,nc2], by=list(alltab[,1]), FUN=fun, na.rm=na.rm) 
					if (counts) {
						cnttab <- aggregate(cnttab[,nc2], by=list(cnttab[,1]), FUN=sum, na.rm=na.rm) 
						if (sdtab) {
							sqtab <- aggregate(sqtab[,nc2], by=list(sqtab[,1]), FUN=sum, na.rm=na.rm) 
						}
					}
				}
				pbStep(pb, i)
			}
			
			alltab <- aggregate(alltab[,nc2], by=list(alltab[,1]), FUN=fun, na.rm=na.rm) 	
			if (counts) {
				cnttab <- aggregate(cnttab[,nc2], by=list(cnttab[,1]), FUN=sum) 
				alltab[nc2] <- alltab[nc2] / cnttab[nc2]
				if (sdtab) {
					sqtab <- aggregate(sqtab[,nc2], by=list(sqtab[,1]), FUN=sum, na.rm=na.rm) 
					alltab[nc2] <- sqrt(( (sqtab[,nc2] / cnttab[,nc2]) - (alltab[nc2])^2 ) * (cnttab[,nc2]/(cnttab[,nc2]-1)))
				}
				
			}
		}
	
		alltab <- as.matrix(alltab)
		colnames(alltab)[1] <- 'zone'
		if (ncol(alltab) > 2) {
			colnames(alltab)[2:ncol(alltab)] <- layernames
		} else {
			colnames(alltab)[2] <- stat[1]
		}
		pbClose(pb)
	
		return(alltab)
	}
)

#zonal(r, z, 'sd')


