# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3



# friendly rbind
.frbind <- function(x, ...) {

	if (! inherits(x, 'data.frame') ) {
		x <- data.frame(x)
	}

	d <- list(...)
	if (length(d) == 0) { return(x) }
	
	for (i in 1:length(d)) {
		
		dd <- d[[i]]
		if (! inherits(dd, 'data.frame')) {
			dd <- data.frame(dd)
		}
		
		cnx <- colnames(x)
		cnd <- colnames(dd)
		
		e <- cnx[(cnx %in% cnd)]	
		for (j in e) {
			if (class(x[,j]) != class(dd[,j])) {
				x[,j] <- as.character(x[,j])
				dd[,j] <- as.character(dd[,j])
			}
		}
		
		a <- which(!cnd %in% cnx)
		if (length(a) > 0) {
			zz <- dd[NULL, a, drop=FALSE]
			zz[1:nrow(x),] <- NA
			x <- cbind(x, zz)
		}

		b <- which(!cnx %in% cnd)
		if (length(b) > 0) {
			zz <- x[NULL, b, drop=FALSE]
			zz[1:nrow(dd),] <- NA
			dd <- cbind(dd, zz)
		}
		
		x <- rbind(x, dd)		
	}
	x
}



#setMethod('c', signature(x='SpatialPolygons'), 

.appendPolygons <- function (x, ..., keepnames=FALSE) {
		
		x <- list(x, ...)
		
		rwn <- lapply(x, row.names)
		ln <- sapply(rwn, length)
		rnu <- raster:::.uniqueNames(unlist(rwn))
		end <- cumsum(ln)
		start <- c(0, end[-length(end)]) + 1
		for (i in 1:length(x)) {
			if (keepnames) {
				if (! all(rnu[start[i]:end[i]] == rwn[[i]]) ) {
					row.names(x[[i]]) <- rnu[start[i]:end[i]]
				}
			} else {
				row.names(x[[i]]) <- as.character(start[i]:end[i])
			}	
		}

		cls <- sapply(x, class)
		if (all(cls == 'SpatialPolygons')) {
			return( do.call( rbind, x))
		}

		if (all(cls == 'SpatialPolygonsDataFrame')) {
			dat <- lapply( x, function(x) { slot(x, 'data') } )
			dat <- do.call(.frbind, dat)
			x <- sapply(x, function(y) as(y, 'SpatialPolygons'))
			x <- do.call( rbind, x)
			rownames(dat) <- row.names(x)
			return( SpatialPolygonsDataFrame(x, dat) )
		}

		
		dat <- NULL
#		dataFound <- FALSE
		for (i in 1:length(x)) {
			if (.hasSlot(x[[i]], 'data')) {
#				dataFound <- TRUE
				if (is.null(dat)) {
					dat <- x[[i]]@data
				} else {
					d <- x[[i]]@data
					cndat <- colnames(dat)
					cnd <- colnames(d)
					p <- cndat[cndat %in% cnd]
					z <- which(!cndat %in% cnd)
					if (length(z) > 1) {
						dd <- dat[NULL, z]
						dd[1:nrow(d),] <- NA
						d <- cbind(d, dd)
					}
					z <- which(!cnd %in% cndat)
					if (length(z) > 1) {
						dd <- d[NULL, z]
						dd[1:nrow(dat),] <- NA
						dat <- cbind(dat, dd)
					}
					
					for (j in p) {
						if (class(dat[,j]) != class(d[,j])) {
							dat[,j] <- as.character(dat[,j])
							d[,j] <- as.character(d[,j])
						}
					}

					
					dat <- rbind(dat, d)
				}
			} else {
				if ( is.null(dat)) {
					dat <- data.frame()
					dat[1:length(x[[i]]@polygons),] <- NA
					rownames(dat) <- row.names(x[[i]])
				} else {
					dd <- dat[NULL, ]
					dd[1:length(x[[i]]@polygons),] <- NA
					rownames(dd) <- row.names(x[[i]])
					dat <- rbind(dat, dd)
				}	
			}
		}
#		if (! dataFound ) { return( do.call(rbind, x) ) }
		x <- sapply(x, function(x) as(x, 'SpatialPolygons'))
		x <- do.call(rbind, x)
		SpatialPolygonsDataFrame(x, dat)
}


