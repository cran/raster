# Author: Robert J. Hijmans
# Date : December 2013
# Version 1.0
# Licence GPL v3

# just for playing right now.


setClass ('VectorLayer',
	representation (
		type = 'character',
		extent = 'Extent',
		source = 'character',
		index = 'matrix',
		holes = 'matrix',
		coordinates = 'matrix',
		attributes = 'data.frame',
		crs = 'CRS',
		history = 'list'
	),
	prototype (	
		type = 'none',
		index = matrix(ncol=4, nrow=0, dimnames=list(NULL, c('object','part','from','to'))),
		holes = matrix(ncol=3, nrow=0, dimnames=list(NULL, c('object','part', 'holeof'))),
		coordinates = matrix(ncol=2, nrow=0, dimnames=list(NULL, c('x','y'))),
		attributes = data.frame(),
		crs = CRS(),
		history = list()
	),
	validity = function(object) {
		methods::validObject(extent(object))
		return(type %in% c('points', 'lines', 'polygons'))
	}
)



setMethod("plot", signature(x='VectorLayer'),
function(x, ...) {
	x <- methods::as(x, 'Spatial')
	plot(x, ...)
}
)

setMethod("spplot", signature(obj='VectorLayer'),
function(obj, ...) {
	obj <- methods::as(obj, 'Spatial')
	spplot(obj, ...)
}
)



setMethod ('show' , 'VectorLayer', 
	function(object) {
		cat('class       :' , class(object), '\n')
		cat('type        :' , object@type, '\n')
		if (nrow(object@index) > 0) {
			cat('features    :' , max(object@index[,1]), '\n')
			cat('parts       :' , nrow(unique(object@index[,1:2])), '\n')
			cat('holes       :' , nrow(object@holes), '\n')
			e <- object@extent
			cat('extent      : ' , e@xmin, ', ', e@xmax, ', ', e@ymin, ', ', e@ymax, '  (xmin, xmax, ymin, ymax)\n', sep="")
			cat('coord. ref. :' , as.character(object@crs), '\n')
		}
		nc <- ncol(object@attributes)
		if (nc > 0) {
			x <- object@attributes
			maxnl <- 15		
			cat('variables   : ', nc, '\n', sep="" ) 
			if (nc > maxnl) {
				x <- x[, 1:maxnl]
			}
			ln <- colnames(x)
			if (nc > maxnl) {
				ln <- c(ln[1:maxnl], '...')
				x <- x[, 1:maxnl]
			}
			wrn <- getOption('warn')
			on.exit(options('warn' = wrn))
			options('warn'=-1) 
			r <- apply(x, 2, range, na.rm=TRUE)
			minv <- as.vector(r[1, ])
			maxv <- as.vector(r[2, ])
			if (nc > maxnl) {
				minv <- c(minv, '...')
				maxv <- c(maxv, '...')
			}

			w <- pmax(nchar(ln), nchar(minv), nchar(maxv))
			m <- rbind(ln, minv, maxv)
				# a loop because 'width' is not recycled by format
			for (i in 1:ncol(m)) {
				m[,i]   <- format(m[,i], width=w[i], justify="right")
			}

			cat('names       :', paste(m[1,], collapse=', '), '\n')
			cat('min values  :', paste(m[2,], collapse=', '), '\n')
			cat('max values  :', paste(m[3,], collapse=', '), '\n')
		}
	}
)	
	

setAs('SpatialPolygons', 'VectorLayer',
	function(from) {
		v <- methods::new('VectorLayer')
		v@type <- 'polygons'
		a <- geom(from)
		v@coordinates <- a[, c('x', 'y')]
		v@extent <- extent(v@coordinates)
		ag <- aggregate(1:nrow(a), a[,1:2], range)
		ag <- as.matrix(ag[order(ag$object, ag$part), ])
		holes <- cbind(as.matrix(unique(a[a$hole>0, 1:2])))
		if (nrow(holes) > 0) {
			holes <- cbind(holes, 1)
			colnames(holes) <- colnames(v@holes)
			v@holes <- holes
		}
		colnames(ag) <- colnames(v@index)
		v@index <- ag
		if (methods::.hasSlot(from, 'data')) {
			v@attributes <- from@data
		}
		v@crs <- from@proj4string
		v
	}
)


setAs('SpatialLines', 'VectorLayer',
	function(from) {
		v <- methods::new('VectorLayer')
		v@type <- 'lines'
		a <- geom(from)
		v@coordinates <- a[, c('x', 'y')]
		v@extent <- extent(v@coordinates)
		ag <- aggregate(1:nrow(a), a[,c(1:2)], range)
		ag <- as.matrix(ag[order(ag$object, ag$part, ag$hole), ])
		colnames(ag) <- colnames(v@index)
		v@index <- ag
		if (methods::.hasSlot(from, 'data')) {
			v@attributes <- from@data
		}
		v@crs <- from@proj4string
		v
	}
)


setAs('SpatialPoints', 'VectorLayer',
	function(from) {
		v <- methods::new('VectorLayer')
		v@type <- 'points'
		a <- geom(from)
		v@coordinates <- a[, c('x', 'y')]
		v@extent <- extent(v@coordinates)
		ag <- aggregate(1:nrow(a), a[,c(1:2)], range)
		ag <- as.matrix(ag[order(ag$object, ag$part), ])
		colnames(ag) <- colnames(v@index)
		v@index <- ag
		if (methods::.hasSlot(from, 'data')) {
			v@attributes <- from@data
		}
		v@crs <- from@proj4string
		v
	}
)


setAs('VectorLayer', 'Spatial', 
	function(from) {
		if (from@type == 'polygons') {
			methods::as(from, 'SpatialPolygons')
		} else if (from@type == 'lines') {
			methods::as(from, 'SpatialLines')
		} else if (from@type == 'points') {
			methods::as(from, 'SpatialPoints')
		}
	}
)


setAs('VectorLayer', 'SpatialPolygons', 
	function(from) {
		stopifnot(from@type == 'polygons') 
		stopifnot(nrow(from@index) > 0) 
		sp <- list()
		obs <- max(from@index[,1])
		for (i in 1:obs) {
			idx <- from@index[from@index[,1]==i, ,drop=FALSE]
			p <- unique(idx[,2])
			pp <- list()
			for (j in 1:length(p)) {
				ss <- from@coordinates[idx[j,3]:idx[j,4], ]
				pp[[j]] <- Polygon(as.matrix(ss))
			}
			sp[[i]] <- Polygons(pp, as.character(i))
		}
		holes <- from@holes
		if (nrow(holes) > 0) {
			for (i in 1:nrow(holes)) {
				sp[[holes[i,1]]]@Polygons[[holes[i,2]]]@hole <- TRUE
			}
		}
		pols <- SpatialPolygons(sp, proj4string=from@crs)
		if (nrow(from@attributes) > 0) {
			att <- from@attributes
			rownames(att) <- 1:obs
			pols <- SpatialPolygonsDataFrame(pols, att)
		}
		return(pols)
	}
)



setAs('VectorLayer', 'SpatialLines', 
	function(from) {
		stopifnot(from@type == 'lines') 
		stopifnot(nrow(from@index) > 0) 
		sp <- list()
		obs <- max(from@index[,1])
		for (i in 1:obs) {
			idx <- from@index[from@index[,1]==i, ,drop=FALSE]
			p <- unique(idx[,2])
			pp <- list()
			for (j in 1:length(p)) {
				ss <- from@coordinates[idx[j,3]:idx[j,4], ]
				pp[[j]] <- Line(as.matrix(ss))
			}
			sp[[i]] <- Lines(pp, as.character(i))
		}
		lns <- SpatialLines(sp, proj4string=from@crs)
		if (nrow(from@attributes) > 0) {
			att <- from@attributes
			rownames(att) <- 1:obs
			pols <- SpatialLinesDataFrame(lns, att)
		}
		return(lns)
	}
)




setAs('VectorLayer', 'SpatialLines', 
	function(from) {
		stopifnot(from@type == 'lines') 
		stopifnot(nrow(from@index) > 0) 
		sp <- list()
		obs <- max(from@index[,1])
		if (obs < nrow(from@index)) {
			stop('multi-point objects are not supported by sp')
		}
		pts <- SpatialPoints(from@coordinates, proj4string=from@crs)
		if (nrow(from@attributes) > 0) {
			att <- from@attributes
			rownames(att) <- 1:obs
			pts <- SpatialPointsDataFrame(pts, att)
		}
		return(pts)
	}
)
