# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3



setMethod('merge', signature(x='Spatial', y='data.frame'), 
function(x, y, by=intersect(names(x), names(y)), by.x=by, by.y=by, all.x=TRUE, suffixes = c(".x",".y"), incomparables = NULL, ...) {
	if (!'data' %in% slotNames(x)) {
		stop('x has no data.frame')
	}
	d <- x@data
	d$donotusethisvariablename976 <- 1:nrow(d)

	y <- unique(y)
	if (!all.x) {
		y$donotusethisvariablename679 <- 1
	}
	d <- merge(d, y, by=by, by.x=by.x, by.y=by.y, suffixes=suffixes, incomprables=incomprable, all.x=TRUE, all.y=FALSE)
	d <- d[order(d$donotusethisvariablename976), ]
	d <- d[, -which(colnames(d)=='donotusethisvariablename976'), drop=FALSE]
	rownames(d) <- row.names(x)
	x@data <- d
	if (! all.x ) {
		x <- x[!is.na(x@data$donotusethisvariablename679),  ,drop=FALSE] 
		x@data <- x@data[ , -which(colnames(x@data)=='donotusethisvariablename679'), drop=FALSE]
	}
	x
} 
)


setMethod('merge', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y, ..., intersect=TRUE) {

	require(rgeos)

	if (!intersect) {
		return( .appendPolygons(x, y, ...) )
	}
	
	row.names(x) <- as.character(1:length(row.names(x)))
		
	yy <- list(...)
	if (length(yy) > 0) {
		keep <- sapply(yy, function(x) inherits(a, 'SpatialPolygons'))
		yy <- yy[keep]
	}
	yy <- c(y, yy)
	
	for (y in yy) {

		if (! identical(projection(x), projection(y)) ) {
			warning('non identical CRS')
			y@proj4string <- x@proj4string
		}	
	
		subs <- gIntersects(x, y, byid=TRUE)
		if (sum(subs)==0) {
			x <- .appendPolygons(x, y)
			next
		}
		y <- spChFIDs(y, as.character(1:length(row.names(y))))

		
		dat <- daty <- datx <- NULL
		xdata <- ydata <- FALSE
		if (.hasSlot(x, 'data')) {
			xdata <- TRUE
			datx <- dat <- x@data[NULL, ,drop=FALSE]
		} 
		if (.hasSlot(y, 'data')) {
			ydata <- TRUE
			daty <- y@data[NULL, ,drop=FALSE]
			if (is.null(dat)) {
				dat <- daty
			} else {
				ynotx <- which(! colnames(daty) %in% colnames(dat))
				if (length(ynotx) > 0) {
					daty <- daty[, ynotx, drop=FALSE]
					dat <- cbind(dat, daty)
				}
				xnoty <- which(! colnames(x@data) %in% colnames(y@data))
				if (length(xnoty) > 0) {
					datx <- dat[, xnoty]
				}
			}
		}

		res <- NULL
		yd <- aggregate(y)
		dif1 <- gDifference(x, yd, byid=TRUE)
		if (!is.null(dif1)) {
			res <- dif1@polygons
			if (xdata) {
				ids <- as.numeric(sapply(row.names(dif1), function(x) strsplit(x, ' ')[[1]][1]))
				#ids <- match(ids, rownames(x@data))
				d <- x@data[ids,]
				rownames(d) <- NULL
				if (ydata & !is.null(daty)) {
					dd <- daty
					dd[1:length(ids),] <- NA
					dat <- cbind(d, dd)
				} else {
					dat <- d
				}
			} else if (ydata) {
				dat[1:length(dif1@polygons),] <- NA
			}
		}

		xd <- aggregate(x)
		dif2 <- gDifference(y, xd, byid=TRUE)
		if (!is.null(dif2)) {
			res <- c(res, dif2@polygons)
			ids <- sapply(row.names(dif2), function(x) strsplit(x, ' ')[[1]][1])
			if (ydata) {
				#ids <- match(ids, rownames(y@data))
				d <- y@data[ids, ,drop=FALSE]
				rownames(d) <- NULL
				if (xdata) {
					dd <- datx
					dd[1:length(ids),] <- NA
					dd <- cbind(d, dd)
					dat <- rbind(dat, dd)
				} else {
					dat <- rbind(dat, d)
				}
			} else if (xdata) {
				dat[(nrow(dat)+1):(nrow(dat)+length(ids)),] <- NA
			}
		}

		subsx <- apply(subs, 2, any)
		subsy <- apply(subs, 1, any)
		
		if (sum(subsx) > 0 ) {
			int  <- gIntersection(x[subsx,], y[subsy,], byid=TRUE)
			if (inherits(int, "SpatialCollections")) {
				int <- int@polyobj
			}
			if (inherits(int, 'SpatialPolygons')) {
				res <- c(res, int@polygons)
				ids <- do.call(rbind, strsplit(row.names(int), ' '))

				d <- NULL
				rows <- (nrow(dat)+1):(nrow(dat)+length(ids[,1]))
				if (xdata) {
					idsx <- match(ids[,1], rownames(x@data))
					d <- x@data[idsx, ,drop=FALSE]
					if (ydata) {
						idsy <- match(ids[,2], rownames(y@data))
						d <- cbind(d, y@data[idsy, ,drop=FALSE])
					}
				} else if (ydata) {
					idsy <- match(ids[,2], rownames(y@data))
					d <- y@data[idsy, ,drop=FALSE]
				}
				dat <- rbind(dat, d)
			}
		}
		res <- SpatialPolygons(lapply(1:length(res), function(y) Polygons(res[[y]]@Polygons, y)))
		if (!is.null(dat)) {
			rownames(dat) <- 1:nrow(dat)
			res <- SpatialPolygonsDataFrame(res, dat)
		}
		res@proj4string <- x@proj4string
		#aggregate(res, v=colnames(res@data))
		x <- res
	}
	
	x	
}
)

