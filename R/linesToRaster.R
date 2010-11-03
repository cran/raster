# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3

.specialRowFromY <- function(object, y) {
	rownr <- 1 + (trunc((ymax(object) - y)/yres(object)))
    rownr[y == ymin(object)] <- nrow(object)
    rownr[y > ymax(object)] <- -1
	rownr[y < ymin(object)] <- nrow(object) + 1
	return(rownr)
}

.specialColFromX <- function(object, x) {
	colnr <- (trunc((x - xmin(object))/xres(object))) + 1
    colnr[x == xmax(object)] <- ncol(object)
    colnr[x < xmin(object)] <- -1 
	colnr[x > xmax(object)] <- ncol(object) + 1
    return(colnr)
}



.getCols <- function(rs, rownr, aline, line1, line2) {
	minx <- xmin(rs)
	maxx <- xmax(rs)
	resxy <- matrix(NA, ncol=2, nrow=0)
	miny <- min(line1[,2], line2[,2])
	maxy <- max(line1[,2], line2[,2])
	xyxy <- cbind(aline[1:(length(aline[,1])-1), ,drop=FALSE], aline[-1, ,drop=FALSE])

    xyxy <- subset(xyxy, !( (xyxy[,2] > maxy & xyxy[,4] > maxy ) | (xyxy[,2] < miny & xyxy[,4] < miny)) )
	if (length(xyxy) < 1) { 
		return(resxy) 
	}
	res <- vector(length=0)
	for (i in 1:length(xyxy[,1])) {	
		rows <- .specialRowFromY(rs, c(xyxy[i,2], xyxy[i,4]) )
		if ((rows[1] > rownr & rows[2] > rownr) | (rows[1] < rownr & rows[2] < rownr)) { 
			next
		}
		cols <- .specialColFromX(rs, c(xyxy[i,1], xyxy[i,3]))
		if ((cols[1] < 1 & cols[2] < 1) | (cols[1] > ncol(rs) & cols[2] > ncol(rs))) { 
			next
		}

		rowcol <- cbind(rows, cols)[order(cols),]
		if (rowcol[1,1] == rowcol[2,1]) {
			# entire line segment in row
			add = rowcol[1,2]:rowcol[2,2]
			add = subset(add, add>0 & add<=ncol(rs))
			res <- c(res, add)
		} else {
			if (rowcol[1,1] == rownr  ) {
				# line segment starts in this row
				if (rowcol[2,1] < rownr) {
					xy <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4]  )
				} else {
					xy <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4]  )
				}
				xy <- t(as.matrix(xy))
				outcol = min(.specialColFromX(rs, xy[,1]), ncol(rs))
				if (outcol < 1) next
				cols <- c(max(1, rowcol[1,2]), outcol)
				col1 <- min(cols)
				col2 <- max(cols)
				res <- c(res, col1:col2)
			} else if (rowcol[2,1] == rownr) {
				# line segment ends in this row
				if (rowcol[1,1] < rownr) {
					xy <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4] )
				} else {
					xy <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4] )
				}
				if (is.na(xy[1])) { next }
				xy <- t(as.matrix(xy))
				incol <- max(1, .specialColFromX(rs, xy[,1]))
				if (incol > ncol(rs)) next
				cols <- c(incol, min(ncol(rs), rowcol[2,2]))
				col1 <- min(cols)
				col2 <- max(cols)
				res <- c(res, col1:col2)
			} else {
				# line segment crosses this row
				xy1 <- .intersectSegments(line1[1,1], line1[1,2], line1[2,1], line1[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4]  )
				xy2 <- .intersectSegments(line2[1,1], line2[1,2], line2[2,1], line2[2,2], xyxy[i,1], xyxy[i,2], xyxy[i,3], xyxy[i,4]  )
				if (is.na(xy1[1])) { next }
				if (is.na(xy2[1])) { next }
				xy <- rbind(xy1, xy2)
				cols <- .specialColFromX(rs, xy[,1])
				col1 <- min(cols)
				col2 <- max(cols)
				if (col1 > ncol(rs)) { next }
				if (col2 == -1) {  next }
				if (col1 == -1) { col1 <- 1 }
				if (col2 > ncol(rs)) { col2 <- ncol(rs) }
				res <- c(res, col1:col2)
			}
		}
	}
	return(res)
}


linesToRaster <- function(lns, raster, field=0, overlap='last', mask=FALSE, updateRaster=FALSE, updateValue="NA", filename="", ...) {

	filename <- trim(filename)

	if (mask & updateRaster) { 
		stop('use either "mask" OR "updateRaster"')
	}
	if (mask) { 
		oldraster <- raster 
	}
	if (updateRaster) {
		oldraster <- raster 
		if (!(updateValue == 'NA' | updateValue == '!NA' | updateValue == 'all' | updateValue == 'zero')) {
			stop('updateValue should be either "all", "NA", "!NA", or "zero"')
		}
	}

	if (!(overlap %in% c('first', 'last', 'sum', 'min', 'max', 'count'))) {
		stop('invalid value for overlap')
	}
	


	raster <- raster(raster)
	if (projection(lns) != "NA") {
		projection(raster) = projection(lns)
	}
	
	if (inherits(lns, 'SpatialPolygons')) {
		lns <- as(lns, "SpatialLines")
	}
	if (! inherits(lns, 'SpatialLines')) {
		stop('lns should be, or inherit from, a SpatialLines* object')
	}

# check if bbox of raster and lns overlap
	spbb <- bbox(lns)
	rsbb <- bbox(raster)
	if (spbb[1,1] > rsbb[1,2] | spbb[2,1] > rsbb[2,2]) {
		stop('lines and raster have no overlapping areas')
	}
	nline <- length(lns@lines)
	info <- matrix(NA, nrow=nline, ncol=3)
	for (i in 1:nline) {
		info[i,1] <- length(lns@lines[[i]]@Lines)
		miny <- NULL
		maxy <- NULL
		for (j in 1:info[i,1]) {
			miny <- min(miny, min(lns@lines[[i]]@Lines[[j]]@coords[,2]))
			maxy <- max(maxy, max(lns@lines[[i]]@Lines[[j]]@coords[,2]))
		}
		info[i,2] <- miny
		info[i,3] <- maxy
	}
	lxmin <- min(spbb[1,1], rsbb[1,1]) - 0.5 * xres(raster)
	lxmax <- max(spbb[1,2], rsbb[1,2]) + 0.5 * xres(raster)

	if (! is.numeric(field) ) {
		field <- which(colnames(lns@data) == field)[1]
		if (is.na(field)) {
			stop('field does not exist')
		}
	} 
	
	if (length(field) > 1) { 
		if (length(field) == nline) {
			putvals <- field
		} else {
			stop('field should be a single value or equal the number of polygons') 
		}	
	} else if (inherits(lns, 'SpatialLines') & overlap == 'sum') {
		putvals <- rep(1, nline)
	} else if ( field < 0) {
		putvals <- rep(1, nline)
	} else if ( field == 0 | class(lns) == 'SpatialLines') {
		putvals <- as.integer(1:nline)
	} else {
		putvals <- as.vector(lns@data[,field])
		if (class(putvals) == 'factor') {
			warning('selected field is factor type')
			putvals <- as.numeric(as.character(putvals))
		}
		if (class(putvals) == 'character') {
			warning('selected field is character type')
			putvals <- as.numeric(putvals)
		}
	}
		

	if (filename == "") {
		v <- matrix(NA, ncol=nrow(raster), nrow=ncol(raster))
	} else {
		raster <- writeStart(raster, filename=filename, ...)
	}
	rv1 <- rep(NA, ncol(raster))
	pb <- pbCreate(nrow(raster), type=.progress(...))
	for (r in 1:nrow(raster)) {
		rv <- rv1
		ly <- yFromRow(raster, r)
		line1 <- rbind(c(lxmin, ly + 0.5*yres(raster)), c(lxmax,ly + 0.5*yres(raster)))
		line2 <- rbind(c(lxmin, ly - 0.5*yres(raster)), c(lxmax,ly - 0.5*yres(raster)))
		uly <- ly + 0.51 * yres(raster)
		lly <- ly - 0.51 * yres(raster)
		for (i in 1:nline) {
			if (info[i,2] > uly | info[i,3] < lly) {
				#  line object is outside of row,  do nothing
			} else {
				for (j in 1:info[i,1]) {
					if ( max ( lns@lines[[i]]@Lines[[j]]@coords[,2] ) < lly  |  min( lns@lines[[i]]@Lines[[j]]@coords[,2] ) > uly ) {
						#  line part entirely outside of row. do nothing
					} else {
						aline <- lns@lines[[i]]@Lines[[j]]@coords
						colnrs <- .getCols(raster, r, aline, line1, line2)
						if ( length(colnrs) > 0 ) {	
							rvtmp <- rv1
							rvtmp[colnrs] <- putvals[i]
							
							if (mask) {
								rv[!is.na(rvtmp)] <- rvtmp[!is.na(rvtmp)]
							} else if (overlap=='last') {
								rv[!is.na(rvtmp)] <- rvtmp[!is.na(rvtmp)]
							} else if (overlap=='first') {
								rv[is.na(rv)] <- rvtmp[is.na(rv)]
							} else if (overlap=='sum') {
								rv[!is.na(rv) & !is.na(rvtmp)] <- rv[!is.na(rv) & !is.na(rvtmp)] + rvtmp[!is.na(rv) & !is.na(rvtmp)] 
								rv[is.na(rv)] <- rvtmp[is.na(rv)]
							} else if (overlap=='min') {
								rv[!is.na(rv) & !is.na(rvtmp)] <- pmin(rv[!is.na(rv) & !is.na(rvtmp)], rvtmp[!is.na(rv) & !is.na(rvtmp)])
								rv[is.na(rv)] <- rvtmp[is.na(rv)]
							} else if (overlap=='max') {
								rv[!is.na(rv) & !is.na(rvtmp)] <- pmax(rv[!is.na(rv) & !is.na(rvtmp)], rvtmp[!is.na(rv) & !is.na(rvtmp)])
								rv[is.na(rv)] <- rvtmp[is.na(rv)]
							} else if (overlap=='count') {
								rvtmp[!is.na(rvtmp)]  <- 1
								rv[!is.na(rv) & !is.na(rvtmp)] <- rv[!is.na(rv) & !is.na(rvtmp)] + rvtmp[!is.na(rv) & !is.na(rvtmp)] 
								rv[is.na(rv)] <- rvtmp[is.na(rv)]				
							}							
						}
					}
				}
			}
		}
		
		
		if (mask) {
			oldvals <- getValues(oldraster, r)
			ind <- which(is.na(rv))
			oldvals[ind] <- NA
			rv <- oldvals
		} else if (updateRaster) {
			oldvals <- getValues(oldraster, r)
			if (updateValue == "all") {
				ind <- which(!is.na(rv))
			} else if (updateValue == "zero") {
				ind <- which(oldvals==0 & !is.na(rv))
			} else if (updateValue == "NA") {
				ind <- which(is.na(oldvals))
			} else {
				ind <- which(!is.na(oldvals) & !is.na(rv))
			}
			oldvals[ind] <- rv[ind]
			rv <- oldvals
		}

		
		if (filename == "") {
			v[,r] <- rv
		} else {
			raster <- writeValues(raster, rv, r)
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

