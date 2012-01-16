# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : January 2009
# Version 0.9
# Licence GPL v3


.intersectSegments <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
# Translated by RH from LISP code by Paul Reiners
# http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/linesegments.lisp
# Which was tranlated from the algorithm by Paul Bourke given here: http://local.wasp.uwa.edu.au/~pbourke/geometry/lineline2d/
    denom  <-  ((y4 - y3) * (x2 - x1)) - ((x4 - x3) * (y2 - y1))
    ua_num  <- ((x4 - x3) *(y1 - y3)) - ((y4 - y3) * (x1 - x3))
    ub_num  <- ((x2 - x1) *(y1 - y3)) - ((y2 - y1) * (x1 - x3))
# If the denominator and numerator for the equations for ua and ub are 0 then the two lines are coincident.
    if ( denom == 0 & ua_num == 0 & ub_num == 0) {
#		return(c(x1, y1))
		xmin <- max(x1, x3)
		if (xmin==x1) {ymin <- y1} else {ymin <- y3}
		xmax <- min(x2, x4)
		if (xmax==x2) {ymax <- y2} else {ymax <- y4}
# RH: for coincident line (segments) returning two intersections : start and end
		return(rbind(c(xmin, ymin),
					 c(xmax, ymax)))
	}	
# If the denominator for the equations for ua and ub is 0 then the two lines are parallel.
    if (denom == 0) {
		return(c(NA, NA))
	}
 	ua <- round(ua_num / denom, 12)
    ub <- round(ub_num / denom, 12)
	if ((ua >= 0 & ua <= 1) & (ub >= 0 & ub <= 1) ) {
        x <- x1 + ua * (x2 - x1)
        y <- y1 + ua * (y2 - y1) 
		return(c(x, y))
	} else {
		return(c(NA, NA))
	}
}


.intersectLinePolygon <- function(line, poly) {
	resxy <- matrix(NA, ncol=2, nrow=0)
	miny <- min(line[,2])
	maxy <- max(line[,2])
	xyxy <- cbind(poly, rbind(poly[-1,], poly[1,]))
    xyxy <- subset(xyxy, !( (xyxy[,2] > maxy & xyxy[,4] > maxy ) | (xyxy[,2] < miny & xyxy[,4] < miny)) )
	if (length(xyxy) < 1) { 
		return(resxy) 
	}
	for (i in 1:length(xyxy[,1])) {
		xy <- .intersectSegments(xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4], line[1,1], line[1,2], line[2,1], line[2,2] )
		if (!is.na(xy[1])) {
			resxy <- rbind(resxy, xy)
		}
	}
	return(resxy)
}



.polygonsToRaster <- function(p, raster, field=0, fun='last', background=NA, mask=FALSE, update=FALSE, updateValue="all", getCover=FALSE, filename="", silent=FALSE, ...) {

	dots <- list(...)
	if (!is.null(dots$overlap)) { stop('argument "overlap" is no longer available. Use "fun"') } 
	if (!is.null(dots$updateRaster)) { stop('argument "updateRaster" is no longer available. Use "update"') } 

		
	if (! inherits(p, 'SpatialPolygons') ) {
		stop('The first argument should be an object of the "SpatialPolygons*" lineage')
	}
						
	filename <- trim(filename)
	if (!canProcessInMemory(raster, 3) && filename == '') {
		filename <- rasterTmpFile()
	}
	
	if (getCover) {
		fun <- 'first'
		mask = FALSE
		update=FALSE
		field=-1
	}

	if (is.character(fun)) {
		if (!(fun %in% c('first', 'last', 'sum', 'min', 'max', 'count'))) {
			stop('invalid value for fun')
		}
		doFun <- FALSE
	} else {
		doFun <- TRUE
	}
	
	if (mask & update) { 
		stop('use either "mask" OR "update"')
	} else if (mask) { 
		oldraster <- raster 
		#update <- TRUE 
	} else if (update) {
		oldraster <- raster 
		if (!is.numeric(updateValue)) {
			if (is.na(updateValue)) {
				updateValue <- 'NA'
			} else if (!(updateValue == 'NA' | updateValue == '!NA' | updateValue == 'all')) {
				stop('updateValue should be either "all", "NA", "!NA"')
			}
		} 
	}
	
	raster <- raster(raster)
	
	if (projection(p) != "NA") {
		projection(raster) = projection(p)
	}

# check if bbox of raster and p overlap
	spbb <- bbox(p)
	rsbb <- bbox(raster)
	if (spbb[1,1] >= rsbb[1,2] | spbb[1,2] <= rsbb[1,1] | spbb[2,1] >= rsbb[2,2] | spbb[2,2] <= rsbb[2,1]) {
		stop('polygon and raster have no overlapping areas')
	}
	
	npol <- length(p@polygons)
	
	if (! is.numeric(field) ) {
		field <- which(colnames(p@data) == field)[1]
		if (is.na(field)) {
			stop('field does not exist')
		}
	} 
		
	if (length(field) > 1) { 
		if (length(field) == npol) {
			putvals <- field
		} else {
			stop('field should be a single value or equal the number of polygons') 
		}
	} else if (mask) {
		putvals <- rep(1, length=npol)	
#	} else if (inherits(p, 'SpatialPolygons') & overlap == 'sum') {
#		putvals <- rep(1, npol)
	} else if (field < 0) {
		putvals <- rep(1, length=npol)	
	} else if (field == 0) {
		putvals <- as.integer(1:npol)
	} else {
		putvals <- as.vector(p@data[[field]])
		if (class(putvals) == 'factor') {
			warning('selected field is factor type')
			putvals <- as.numeric(as.character(putvals))
		}
		if (class(putvals) == 'character') {
			warning('selected field is character type')
			putvals <- as.numeric(putvals)
		}
	}

	polinfo <- matrix(NA, nrow=npol * 2, ncol=6)
	addpol <- matrix(NA, nrow=500, ncol=6)

	pollist <- list()
	cnt <- 0
	for (i in 1:npol) {
		nsubpol <- length(p@polygons[[i]]@Polygons)
		for (j in 1:nsubpol) {
			cnt <- cnt + 1
			if (cnt > dim(polinfo)[1]) { 
				polinfo <- rbind(polinfo, addpol)  
			}
			polinfo[cnt, 1] <- cnt
			polinfo[cnt, 2] <- min(p@polygons[[i]]@Polygons[[j]]@coords[,2])
			polinfo[cnt, 3] <- max(p@polygons[[i]]@Polygons[[j]]@coords[,2])
			polinfo[cnt, 4] <- putvals[i]
			if ( p@polygons[[i]]@Polygons[[j]]@hole ) {
				polinfo[cnt, 5] <- 1
			} else {
				polinfo[cnt, 5] <- 0
			}
			polinfo[cnt, 6] <- i
			pollist[cnt] <- p@polygons[[i]]@Polygons[[j]]
		}
	}
	
	if (! silent) {  cat('Found', npol, 'region(s) and', cnt, 'polygon(s)\n') }
	polinfo <- subset(polinfo, polinfo[,1] <= cnt, drop=FALSE)
#	polinfo <- polinfo[order(polinfo[,1]),]
	rm(p)

		
	lxmin <- min(spbb[1,1], rsbb[1,1]) - xres(raster)
	lxmax <- max(spbb[1,2], rsbb[1,2]) + xres(raster)
	if (getCover) { return (.polygoncover(raster, filename, polinfo, lxmin, lxmax, pollist, ...)) }

	adj <- 0.5 * xres(raster)

	if (filename == "") {
		v <- matrix(NA, ncol=nrow(raster), nrow=ncol(raster))
	} else {
		raster <- writeStart(raster, filename=filename, ...)
	}

	rxmn <- xmin(raster) 
	rxmx <- xmax(raster) 
	rv1 <- rep(NA, ncol(raster))
	lst1 <- vector(length=ncol(raster), mode='list')
	holes1 <- rep(FALSE, ncol(raster))
	pb <- pbCreate(nrow(raster), ...)

	for (r in 1:nrow(raster)) {
		if (doFun) {
			rrv <- rv <- lst1
		} else {
			rrv <- rv <- rv1
		}
		ly <- yFromRow(raster, r)
		myline <- rbind(c(lxmin,ly), c(lxmax,ly))
		holes <- holes1
		subpol <- subset(polinfo, !(polinfo[,2] > ly | polinfo[,3] < ly), drop=FALSE)
		if (length(subpol[,1]) > 0) { 		
			updateHoles <- FALSE
			lastpolnr <- subpol[1,6]
			for (i in 1:length(subpol[,1])) {
				if (i == nrow(subpol)) { 
					updateHoles <- TRUE 
				} else if (subpol[i+1,6] > lastpolnr) { # new polygon
					updateHoles <- TRUE 
					lastpolnr <- subpol[i+1,6]
				}
				
				mypoly <- pollist[[subpol[i,1]]]
				intersection <- .intersectLinePolygon(myline, mypoly@coords)
				x <- sort(intersection[,1])
				if (length(x) > 0) {
					rvtmp <- rv1
					if ( sum(x[-length(x)] == x[-1]) > 0 ) {
					# single node intersection going out of polygon ....
						spPnts <- xyFromCell(raster, cellFromRowCol(raster, rep(r, ncol(raster)), 1:ncol(raster)), TRUE)
						spPol <- SpatialPolygons(list(Polygons(list(mypoly), 1)))
						over <- overlay(spPnts, spPol)
						if ( subpol[i, 5] == 1 ) {
							holes[!is.na(over)] <- TRUE
						} else {
							rvtmp[!is.na(over)] <- subpol[i,4] 
						}
						# print(paste('exit node intersection on row:', r))
					} else {
						for (k in 1:round(nrow(intersection)/2)) {
							l <- (k * 2) - 1		
							x1 <- x[l]
							x2 <- x[l+1]
							#if (is.na(x2)) { 
							#	txt <- paste('something funny at row:', r, 'polygon:',j)
							#	stop(txt)
							#}
							#  if (x1 > rxmx) { next }
							# if (x2 < rxmn) { next }
							# adjust to skip first cell if the center is not covered by this polygon
							x1a <- x1 + adj
							x2a <- x2 - adj
							if (x1a > rxmx) { next }
							if (x2a < rxmn) { next }
							x1a <- min(rxmx, max(rxmn, x1a))
							x2a <- min(rxmx, max(rxmn, x2a))
							col1 <- colFromX(raster, x1a)
							col2 <- colFromX(raster, x2a)
							if (col1 > col2) { next }
							if ( subpol[i, 5] == 1 ) {
								holes[col1:col2] <- TRUE
							} else {
								rvtmp[col1:col2] <- subpol[i,4]
							}
						}
					}
					
		
					if (doFun) {
						ind <- which(!is.na(rvtmp))
						for (ii in ind) {
							rv[[ii]] <- c(rv[[ii]], rvtmp[ii])
						}
					} else if (mask) {
						rv[!is.na(rvtmp)] <- rvtmp[!is.na(rvtmp)]
					} else if (fun=='last') {
						rv[!is.na(rvtmp)] <- rvtmp[!is.na(rvtmp)]
					} else if (fun=='first') {
						rv[is.na(rv)] <- rvtmp[is.na(rv)]
					} else if (fun=='sum') {
						rv[!is.na(rv) & !is.na(rvtmp)] <- rv[!is.na(rv) & !is.na(rvtmp)] + rvtmp[!is.na(rv) & !is.na(rvtmp)] 
						rv[is.na(rv)] <- rvtmp[is.na(rv)]
					} else if (fun=='min') {
						rv[!is.na(rv) & !is.na(rvtmp)] <- pmin(rv[!is.na(rv) & !is.na(rvtmp)], rvtmp[!is.na(rv) & !is.na(rvtmp)])
						rv[is.na(rv)] <- rvtmp[is.na(rv)]
					} else if (fun=='max') {
						rv[!is.na(rv) & !is.na(rvtmp)] <- pmax(rv[!is.na(rv) & !is.na(rvtmp)], rvtmp[!is.na(rv) & !is.na(rvtmp)])
						rv[is.na(rv)] <- rvtmp[is.na(rv)]
					} else if (fun=='count') {
						rvtmp[!is.na(rvtmp)]  <- 1
						rv[!is.na(rv) & !is.na(rvtmp)] <- rv[!is.na(rv) & !is.na(rvtmp)] + rvtmp[!is.na(rv) & !is.na(rvtmp)] 
						rv[is.na(rv)] <- rvtmp[is.na(rv)]				
					}
				}
				if (updateHoles) {
					if (doFun) {
						tmp <- rv
						rv <- lst1
						ind <- which(!holes )
						for (ii in ind) {
							if (!is.null(tmp[[ii]])) {
								rv[[ii]] <- tmp[[ii]]
							}
						}
					} else {
						rv[holes] <- NA
					}
					rrv[!is.na(rv)] <- rv[!is.na(rv)]
					holes <- holes1
					updateHoles = FALSE	
					
				}		
			}
		}
		
		if (doFun) {
			for (i in 1:length(rrv)) {
				if (is.null(rrv[[i]])) {
					rrv[[i]] <- NA
				}
			}
			rrv <- sapply(rrv, fun)
		}
		
		if (mask) {
			oldvals <- getValues(oldraster, r)
			ind <- which(is.na(rrv))
			oldvals[ind] <- NA
			rrv <- oldvals
		} else if (update) {
			oldvals <- getValues(oldraster, r)
			if (is.numeric(updateValue)) {
				ind <- which(oldvals == updateValue & !is.na(rrv))
			} else if (updateValue == "all") {
				ind <- which(!is.na(rrv))
			} else if (updateValue == "NA") {
				ind <- which(is.na(oldvals))
			} else { "!NA"
				ind <- which(!is.na(oldvals) & !is.na(rrv))
			}
			oldvals[ind] <- rrv[ind]
			rrv <- oldvals
		} else {
			rrv[is.na(rrv)] <- background
		}

		if (filename == "") {
			v[,r] <- rrv
		} else {
			raster <- writeValues(raster, rrv, r)
		}
		pbStep(pb, r)
	}
	pbClose(pb)

	if (filename == "") {
		raster <- setValues(raster, as.vector(v))
	} else {
		raster <- writeStop(raster)
	}
	return(raster)
}


.polygoncover <- function(raster, filename, polinfo, lxmin, lxmax, pollist, ...) {
# percentage cover per grid cell
	bigraster <- raster(raster)
	rxmn <- xmin(bigraster) 
	rxmx <- xmax(bigraster) 
	f <- 10
	adj <- 0.5 * xres(bigraster)/f
	nc <- ncol(bigraster) * f
	rv1 <- rep(0, nc)
	holes1 <- rep(FALSE, nc)
	prj <- projection(bigraster)
	hr <- 0.5 * yres(bigraster)

	vv <- matrix(ncol=f, nrow=nc)
	
	if (filename == "") {
		v <- matrix(NA, ncol=nrow(bigraster), nrow=ncol(bigraster))
	} else {
		bigraster <- writeStart(bigraster, filename=filename, ...)
	}
	
	pb <- pbCreate(nrow(bigraster), ...)
	for (rr in 1:nrow(bigraster)) {
		y <- yFromRow(bigraster, rr)
		yn <- y - hr
		yx <- y + hr
		raster <- raster(xmn=rxmn, xmx=rxmx, ymn=yn, ymx=yx, ncols=nc, nrows=f, crs=prj)
		subpol <- subset(polinfo, !(polinfo[,2] > yx | polinfo[,3] < yn), drop=FALSE)
		for (r in 1:f) {
			rv <- rv1
			ly <- yFromRow(raster, r)
			myline <- rbind(c(lxmin,ly), c(lxmax,ly))
			holes <- holes1
			if (length(subpol[,1]) > 0) { 		
				updateHoles = FALSE
				lastpolnr <- subpol[1,6]
				for (i in 1:length(subpol[,1])) {
					if (i == length(subpol[,1])) { 
						updateHoles = TRUE 
					} else if (subpol[i+1,6] > lastpolnr) {
						updateHoles = TRUE 
						lastpolnr <- subpol[i+1,6]
					}
					
					mypoly <- pollist[[subpol[i,1]]]
					intersection <- .intersectLinePolygon(myline, mypoly@coords)
					x <- sort(intersection[,1])
					if (length(x) > 0) {
						rvtmp <- rv1
						if ( sum(x[-length(x)] == x[-1]) > 0 ) {
					# single node intersection going out of polygon ....
							spPnts <- xyFromCell(raster, cellFromRowCol(raster, rep(r, ncol(raster)), 1:ncol(raster)), TRUE)
							spPol <- SpatialPolygons(list(Polygons(list(mypoly), 1)))
							over <- overlay(spPnts, spPol)
							if ( subpol[i, 5] == 1 ) {
								holes[!is.na(over)] <- TRUE
							} else {
								rvtmp[!is.na(over)] <- subpol[i,4] 
							}
						} else {
							for (k in 1:round(nrow(intersection)/2)) {
								l <- (k * 2) - 1		
								x1 <- x[l]
								x2 <- x[l+1]
								if (x1 > rxmx) { next }
								if (x2 < rxmn) { next }
							# adjust to skip first cell if the center is not covered by this polygon
								x1a <- x1 + adj
								x2a <- x2 - adj
								x1a <- min(rxmx, max(rxmn, x1a))
								x2a <- min(rxmx, max(rxmn, x2a))
								col1 <- colFromX(raster, x1a)
								col2 <- colFromX(raster, x2a)
								if (col1 > col2) { next }
								if ( subpol[i, 5] == 1 ) {
									holes[col1:col2] <- TRUE
								} else {
									rvtmp[col1:col2] <- subpol[i,4]
								}
							}
						}
						rv <- pmax(rv, rvtmp)
					}
					if (updateHoles) {
						rv[holes] <- 0
						holes <- holes1
						updateHoles = FALSE	
					}
				}
			}
			vv[,r] <- rv
		}
		av <- colSums( matrix( rowSums(vv), nrow=f) )
		
		if (filename == "") {
			v[,rr] <- av
		} else {
			bigraster <- writeValues(bigraster, av, rr)
		}
		pbStep(pb, rr)
	}
	pbClose(pb)

	if (filename == "") {
		bigraster <- setValues(bigraster, as.vector(v))
	} else {
		bigraster <- writeStop(bigraster)
	}
	return(bigraster)
}




.polygonsToRaster2 <- function(p, raster, field=0, filename="", ...) {
#  This is based on sampling by points. Should be slower except when  polygons very detailed and raster  has low resolution
# but it could be optimized further
# currently not used. Perhaps it should be used under certain conditions. 
# this version does not deal with polygon holes 

# check if bbox of raster and p overlap
	filename <- trim(filename)
	raster <- raster(raster)
	
	spbb <- bbox(p)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('polygon and raster have no overlapping areas')
	}

	if (class(p) == 'SpatialPolygons' | field == 0) {
		putvals <- as.integer(1:length(p@polygons))
	} else {
		putvals <- as.vector(p@data[,field])
		if (class(putvals) == 'character') {
			stop('selected field is charater type')
		}
	}
	
	
	if (filename == "") {
		v <- vector(length=0) # replace this
	} else {
		raster <- writeStart(raster, filename=filename, ...)
	}
	
	rowcol <- cbind(0, 1:ncol(raster))

	firstrow <- rowFromY(raster, spbb[2,2])
	lastrow <- rowFromY(raster, spbb[2,1])
	
	for (r in 1:nrow(raster)) {
		if (r < firstrow | r > lastrow) {
			vals <- rep(NA, times=ncol(raster))
		} else {
			rowcol[,1] <- r
			sppoints <- xyFromCell(raster, cellFromRowCol(raster, rowcol[,1], rowcol[,2]), TRUE)
			over <- overlay(sppoints, p)
			vals <- putvals[over]
		}
		if (filename == "") {
			v <- c(v, vals)
		} else {
			raster <- writeValues(raster, vals)
		}
	}
	if (filename == "") {
		raster <- setValues(raster, v)
	} else {
		raster <- writeStop(raster)
	}
	return(raster)
}

