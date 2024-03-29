# Author: Robert J. Hijmans
# Date : December 2011
# Version 1.0
# Licence GPL v3


#.checkGEOS <- function() {
#	stopifnot(requireNamespace("rgeos"))
#	gval <- rgeos::get_RGEOS_CheckValidity()
#	rgeos::set_RGEOS_CheckValidity(2L)
#	gval
#}


setMethod('intersect', signature(x='SpatialPolygons', y='SpatialPolygons'), 
function(x, y) {

	# warning("this method will be removed. You can use 'terra::intersect<SpatVector,SpatVector>' instead")
	as(intersect(vect(x), vect(y)), "Spatial")

	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
	# prj <- x@proj4string
	# if (is.na(prj)) prj <- y@proj4string
	# x@proj4string <- sp::CRS(as.character(NA))
	# y@proj4string <- sp::CRS(as.character(NA))

# #	threshold <- get_RGEOS_polyThreshold()
# #	on.exit(set_RGEOS_polyThreshold(threshold))
# #	minarea <- min(apply(bbox(union(extent(x), extent(y))), 1, diff) / 1000000, 0.00001)
# #	set_RGEOS_polyThreshold(minarea)
# #	slivers <- get_RGEOS_dropSlivers()
# #	on.exit(set_RGEOS_dropSlivers(slivers))
# #	set_RGEOS_dropSlivers(TRUE)
    

	# x <- sp::spChFIDs(x, as.character(1:length(x)))
	# y <- sp::spChFIDs(y, as.character(1:length(y)))
		
	# subs <- rgeos::gIntersects(x, y, byid=TRUE)
	# if (sum(subs)==0) {
		# warning('polygons do not intersect')
		# return(NULL)
	# }
		
	# xdata <-.hasSlot(x, 'data')
	# ydata <-.hasSlot(y, 'data')
	# dat <- NULL
	# if (xdata & ydata) {
		# nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
		# colnames(x@data) <- xnames <- nms[1:ncol(x@data)]
		# colnames(y@data) <- ynames <- nms[(ncol(x@data)+1):length(nms)]
		# dat <- cbind(x@data[NULL, ,drop=FALSE], y@data[NULL, ,drop=FALSE])
	# } else if (xdata) {
		# dat <- x@data[NULL, ,drop=FALSE]
		# xnames <- colnames(dat)
	# } else if (ydata) {
		# dat <- y@data[NULL, ,drop=FALSE]
		# ynames <- colnames(dat)
	# }

	# subsx <- apply(subs, 2, any)
	# subsy <- apply(subs, 1, any)
		
	# int  <- rgeos::gIntersection(x[subsx,], y[subsy,], byid=TRUE, drop_lower_td=TRUE)
# #	if (inherits(int, "SpatialCollections")) {
# #		if (is.null(int@polyobj)) { # merely touching, no intersection
# #			#warning('polygons do not intersect')
# #			return(NULL)
# #		}
# #		int <- int@polyobj
# #	}
	# if (!inherits(int, 'SpatialPolygons')) {
		# # warning('polygons do not intersect')
		# return(NULL)
	# }

	# if (!is.null(dat)) {
		# ids <- do.call(rbind, strsplit(row.names(int), ' '))
		# rows <- 1:length(ids[,1])
		# if (xdata) {
			# idsx <- match(ids[,1], rownames(x@data))
			# dat[rows, xnames] <- x@data[idsx, ]
		# } 
		# if (ydata) {
			# idsy <- match(ids[,2], rownames(y@data))
			# dat[rows, ynames] <- y@data[idsy, ]
		# }
		# rownames(dat) <- 1:nrow(dat)
		# int <- sp::spChFIDs(int, as.character(1:nrow(dat)))
		# int <- sp::SpatialPolygonsDataFrame(int, dat)
	# }
	
	# if (length(int) > 0) {
	
		# w <- getOption('warn')
		# on.exit(options('warn' = w))
		# options('warn'=-1) 
	
		# j <- rgeos::gIsValid(int, byid=TRUE, reason=FALSE)
		# if (!all(j)) {
			# bad <- which(!j)
			# for (i in bad) {
				# # it could be that a part of a polygon is a sliver, but that other parts are OK
				# a <- sp::disaggregate(int[i, ])
				# if (length(a) > 1) {
					# jj <- which(rgeos::gIsValid(a, byid=TRUE, reason=FALSE))
					# a <- a[jj, ]
					# if (length(a) > 0) {
						# int@polygons[i] <- aggregate(a)@polygons
						# j[i] <- TRUE
					# }
				# } 
			# }
			# int <- int[j,]
		# }		
	# }
	# int@proj4string <- prj
	# int	
} 
)


setMethod('intersect', signature(x='SpatialPolygons', y='SpatialLines'), 
function(x, y) {

	# warning("this method will be removed. You can use 'terra::intersect<SpatVector,SpatVector>' instead")
	as(intersect(vect(x), vect(y)), "Spatial")

	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
	# gval <- rgeos::get_RGEOS_CheckValidity()
	# if (gval != 2) {
		# on.exit(rgeos::set_RGEOS_CheckValidity(gval))
		# rgeos::set_RGEOS_CheckValidity(2L)
	# }

	# prj <- x@proj4string
	# if (is.na(prj)) prj <- y@proj4string
	# x@proj4string <- sp::CRS(as.character(NA))
	# y@proj4string <- sp::CRS(as.character(NA))

	
	# subs <- rgeos::gIntersects(x, y, byid=TRUE)
	# if (sum(subs)==0) {
		# warning('lines and polygons do not intersect')
		# return(NULL)
	# }

	# if (inherits(x, "Spatial")) { x@proj4string <- prj }
	# i <- which(apply(subs, 2, any))
	# x[i, ]
}
)

setMethod('intersect', signature(x='SpatialLines', y='SpatialPolygons'), 
function(x, y) {

	# warning("this method will be removed. You can use 'terra::intersect<SpatVector,SpatVector>' instead")
	as(intersect(vect(x), vect(y)), "Spatial")

	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
	# gval <- rgeos::get_RGEOS_CheckValidity()
	# if (gval != 2) {
		# on.exit(rgeos::set_RGEOS_CheckValidity(gval))
		# rgeos::set_RGEOS_CheckValidity(2L)
	# }
	
	# prj <- x@proj4string
	# if (is.na(prj)) prj <- y@proj4string
	# x@proj4string <- sp::CRS(as.character(NA))
	# y@proj4string <- sp::CRS(as.character(NA))

	# x <- sp::spChFIDs(x, as.character(1:length(x)))
	# y <- sp::spChFIDs(y, as.character(1:length(y)))
		
	# if (! identical( .proj4string(x), .proj4string(y)) ) {
		# warning('non identical crs')
		# y@proj4string <- x@proj4string
	# }	
	
	# subs <- rgeos::gIntersects(x, y, byid=TRUE)
	# if (sum(subs)==0) {
		# warning('lines and polygons do not intersect')
		# return(NULL)
	# }
		
	# xdata <-.hasSlot(x, 'data')
	# ydata <-.hasSlot(y, 'data')
	# dat <- NULL
	# if (xdata & ydata) {
		# nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
		# colnames(x@data) <- xnames <- nms[1:ncol(x@data)]
		# colnames(y@data) <- ynames <- nms[(ncol(x@data)+1):length(nms)]
		# dat <- cbind(x@data[NULL, ,drop=FALSE], y@data[NULL, ,drop=FALSE])
	# } else if (xdata) {
		# dat <- x@data[NULL, ,drop=FALSE]
		# xnames <- colnames(dat)
	# } else if (ydata) {
		# dat <- y@data[NULL, ,drop=FALSE]
		# ynames <- colnames(dat)
	# }

	# subsx <- apply(subs, 2, any)
	# subsy <- apply(subs, 1, any)
		
	# int  <- rgeos::gIntersection(x[subsx,], y[subsy,], byid=TRUE, drop_lower_td=TRUE)
# #	if (inherits(int, "SpatialCollections")) {
# #		if (is.null(int@polyobj)) { # merely touching, no intersection
# #			#warning('polygons do not intersect')
# #			return(NULL)
# #		}
# #		int <- int@polyobj
# #	}
	# if (!inherits(int, 'SpatialLines')) {
		# # warning('polygons do not intersect')
		# return(NULL)
	# }

	# if (!is.null(dat)) {
		# ids <- do.call(rbind, strsplit(row.names(int), ' '))
		# rows <- 1:length(ids[,1])
		# if (xdata) {
			# idsx <- match(ids[,1], rownames(x@data))
			# dat[rows, xnames] <- x@data[idsx, ]
		# } 
		# if (ydata) {
			# idsy <- match(ids[,2], rownames(y@data))
			# dat[rows, ynames] <- y@data[idsy, ]
		# }
		# rownames(dat) <- 1:nrow(dat)
		# int <- sp::spChFIDs(int, as.character(1:nrow(dat)))
		# int <- sp::SpatialLinesDataFrame(int, dat)
	# }
	
	# if (length(int) > 0) {
		# j <- which(rgeos::gIsValid(int, byid=TRUE, reason=FALSE))
		# int <- int[j, ]
	# }
	# int@proj4string <- prj
	# int	
} 
)


setMethod('intersect', signature(x='SpatialLines', y='SpatialLines'), 
function(x, y) {

	# warning("this method will be removed. You can use 'terra::intersect<SpatVector,SpatVector>' instead")
	as(intersect(vect(x), vect(y)), "Spatial")


	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
	# gval <- rgeos::get_RGEOS_CheckValidity()
	# if (gval != 2) {
		# on.exit(rgeos::set_RGEOS_CheckValidity(gval))
		# rgeos::set_RGEOS_CheckValidity(2L)
	# }

	# prj <- x@proj4string
	# if (is.na(prj)) prj <- y@proj4string
	# x@proj4string <- sp::CRS(as.character(NA))
	# y@proj4string <- sp::CRS(as.character(NA))
	
	# xdata <-.hasSlot(x, 'data')
	# ydata <-.hasSlot(y, 'data')

	# x <- sp::spChFIDs(x, as.character(1:length(x)))
	# y <- sp::spChFIDs(y, as.character(1:length(y)))

	# if (! any(c(xdata, ydata))) {
		# z <- rgeos::gIntersection(x, y, byid=TRUE)
		# if (is.null(z)) {
			# z <- sp::SpatialPoints(cbind(0,0),  proj4string=prj)
			# z <- sp::SpatialPointsDataFrame(z,data.frame(x=0, y=0))
			# return( z[-1, ] )
		# }
		# rn <- rownames(z@coords)
		# d <- data.frame(matrix(as.integer(unlist(strsplit(rn, ' '))), ncol=2, byrow=TRUE))
		# colnames(d) <- c('x', 'y')
		# rownames(z@coords) <- NULL
		# z <- sp::SpatialPointsDataFrame(z, d)
		# z@proj4string <- prj
		# return(z)
	# }
	
	# z <- rgeos::gIntersection(y, x, byid=TRUE)
	
	# if (is.null(z)) {
		# z <- sp::SpatialPoints(cbind(0,0),  proj4string=prj)
		# return( z[-1, ] )
	# }
	
	# if (inherits(z, 'SpatialCollections')) {
		# z <- z@pointobj
	# }
	
	# s <- strsplit(sp::spChFIDs(z), ' ')
	# s <- matrix(as.integer(unlist(s)), ncol=2, byrow=TRUE)
	
	# if (xdata & ydata) {
		# nms <- .goodNames(c(colnames(x@data), colnames(y@data)))
		# xnames <- nms[1:ncol(x@data)]
		# ynames <- nms[(ncol(x@data)+1):length(nms)]
		# xd <- x@data[s[,2], ]
		# yd <- y@data[s[,1], ]
		# d <- cbind(xd, yd)
		# colnames(d) <- c(xnames, ynames)
	# } else if (xdata) {
		# d <- x@data[s[,2], ]
	# } else if (ydata) {
		# d <- y@data[s[,1], ]
	# }
	# row.names(d) <- NULL
	# row.names(z) <- as.character(1:length(z))
	# z@proj4string <- prj
	
	# sp::SpatialPointsDataFrame(z, d)
}
)



setMethod('intersect', signature(x='SpatialPolygons', y='SpatialPoints'), 
function(x, y) {

	# warning("this method will be removed. You can use 'terra::intersect<SpatVector,SpatVector>' instead")
	as(intersect(vect(x), vect(y)), "Spatial")
	
	# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
	# gval <- rgeos::get_RGEOS_CheckValidity()
	# if (gval != 2) {
		# on.exit(rgeos::set_RGEOS_CheckValidity(gval))
		# rgeos::set_RGEOS_CheckValidity(2L)
	# }

	# prj <- x@proj4string
	# if (is.na(prj)) prj <- y@proj4string
	# x@proj4string <- sp::CRS(as.character(NA))
	# y@proj4string <- sp::CRS(as.character(NA))
	
	
	# i <- rgeos::gIntersects(x, y, byid=TRUE)
	# i <- which(apply(i, 2, any))
	# if (inherits(x, "Spatial")) { x@proj4string <- prj }
	# x[i, ]
}
)


setMethod('intersect', signature(x='SpatialPoints', y='ANY'), 
function(x, y) {


	# warning("this method will be removed. You can use 'terra::intersect<SpatVector,SpatVector>' instead")
	
	if (inherits(y, 'SpatialLines')) {
		stop('intersect of SpatialPoints and Lines is not supported because of numerical inaccuracies.\nUse "buffer", to create polygons from the lines and use these in intersect')
	}
	
	if (inherits(y, 'SpatialPolygons')) {

		as(intersect(vect(x), vect(y)), "Spatial")


		# valgeos <- .checkGEOS(); on.exit(rgeos::set_RGEOS_CheckValidity(valgeos))
		# gval <- rgeos::get_RGEOS_CheckValidity()
		# if (gval != 2) {
			# on.exit(rgeos::set_RGEOS_CheckValidity(gval))
			# rgeos::set_RGEOS_CheckValidity(2L)
		# }
		# prj <- x@proj4string
		# if (is.na(prj)) prj <- y@proj4string
		# x@proj4string <- sp::CRS(as.character(NA))
		# y@proj4string <- sp::CRS(as.character(NA))

		# i <- rgeos::gIntersects(y, x, byid=TRUE)
	
		# j <- cbind(1:length(y), rep(1:length(x), each=length(y)), as.vector(t(i)))
		# j <- j[j[,3] == 1, -3, drop=FALSE]
		# j <- j[order(j[,2]), ,drop=FALSE]
		# x <- x[j[,2], ]
		
		# if (.hasSlot(y, 'data')) {
			# d <- y@data[j[,1], ]
			# if (!.hasSlot(x, 'data')) {
				# x <- sp::SpatialPointsDataFrame(x, d)
			# } else {
				# x@data <- cbind(x@data, d)
			# }
		# } 
		# x@proj4string <- prj
		# return(x)
		
	} else {
		y <- extent(y)
		xy <- sp::coordinates(x)[,1:2,drop=FALSE]
		i <- xy[,1] >= y@xmin & xy[,1] <= y@xmax & xy[,2] >= y@ymin & xy[,2] <= y@ymax
		x[i, ]
	}
}
)




setMethod('intersect', signature(x='SpatialPolygons', y='ANY'), 
function(x, y) {
	y <- extent(y)
	y <- as(y, 'SpatialPolygons')
	intersect(x, y)
}
)
