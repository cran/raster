# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

.gdFixGeoref <- function(gdalinfo) {
	gdversion = getGDALVersionInfo()
	gdversion = trim(substr(gdversion, 5, 10))
	test <- gdversion < '1.8.0'	
	if (test) {
		v <- attr(gdalinfo, 'mdata')
		if (! is.null(v) ) {
			for (i in 1:length(v)) {
				if (v[i] == "AREA_OR_POINT=Area") {
					return(FALSE)
				} else if (v[i] == "AREA_OR_POINT=Point") {
					return(TRUE)
				}
			}
		}
	}
	return(FALSE)
}



.rasterFromGDAL <- function(filename, band, type, RAT=FALSE, silent=TRUE) {	

	if (! .requireRgdal() ) { stop('package rgdal is not available') }

	# suppressing the geoTransform warning...
	w <- getOption('warn')
	on.exit(options('warn'= w))
	options('warn'=-1) 
	
	if (packageDescription('rgdal')$Version > '0.6-28'  &  RAT) {
		gdalinfo <- do.call(GDALinfo, list(filename, silent=silent, returnRAT=TRUE))
	} else {
		gdalinfo <- do.call(GDALinfo, list(filename, silent=silent))
	}
		
	options('warn'= w) 

	nc <- as.integer(gdalinfo[["columns"]])
	nr <- as.integer(gdalinfo[["rows"]])

	xn <- gdalinfo[["ll.x"]]
	xn <- round(xn, digits=9)

	xx <- xn + gdalinfo[["res.x"]] * nc
	xx <- round(xx, digits=9)

	yn <- gdalinfo[["ll.y"]]
	yn <- round(yn, digits=9)
	yx <- yn + gdalinfo[["res.y"]] * nr
	yx <- round(yx, digits=9)


	rotated <- FALSE
	obx <- gdalinfo[["oblique.x"]]
	oby <- gdalinfo[["oblique.y"]]
	if (obx != 0 | oby != 0) {
		gd <- GDAL.open(filename)
		geoTrans <- .Call("RGDAL_GetGeoTransform", gd, PACKAGE = "rgdal")
		GDAL.close(gd)

		## adapted from rgdal::getGeoTransFunc
		if (attr(geoTrans, "CE_Failure")) {
			stop("Rotated values in file, but GeoTransform values not available")
		}
		warning('\n\n This file has a rotation\n Support such files is limited and results of data processing might be wrong.\n Proceed with caution & consider using the "rectify" function\n')

		rotMat <- matrix(geoTrans[c(2, 3, 5, 6)], 2)
		invMat <- solve(rotMat)
		
		offset <- geoTrans[c(1, 4)]
		gt <- function(x, inv=FALSE) {
			if (inv) {
				x <- t(t(x) - c(offset[1], offset[2]))
				x <- round( x %*% invMat  + 0.5 )
				x[x < 1] <- NA
				x[x[,1] > nc  | x[,2] > nr, ] <- NA
			} else {
				x <- (x - 0.5) %*% rotMat
				x <- t(t(x) + c(offset[1], offset[2])) 
			}
			return(x)
		}
		## end adpated from rgdal::getGeoTransFunc
		
		crd <- gt(cbind(c(0, 0, nc, nc), c(0, nr, 0, nr))+0.5)
		rot <- new(".Rotation")
		rot@geotrans <- as.vector(geoTrans)
		rot@transfun <- gt
		#rot@upperleft <- crd[1,]
		#rot@lowerleft <- crd[2,]
		#rot@upperright <- crd[3,]
		#rot@lowerright <- crd[4,]
		rotated <- TRUE

		xn  <- min(crd[,1])
		xx  <- max(crd[,1])
		yn  <- min(crd[,2])
		yx  <- max(crd[,2])
		
	} 
	
	
	fixGeoref <- FALSE
	try( fixGeoref <- .gdFixGeoref(gdalinfo), silent=TRUE )
	
	if (type == 'RasterBrick') {
		x <- brick(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs="")
		x@data@nlayers <- as.integer(gdalinfo[["bands"]])
		x@file@nbands <- as.integer(gdalinfo[["bands"]])
	} else {
		x <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs="")
		x@file@nbands <- as.integer(gdalinfo[["bands"]])
		band <- as.integer(band)
		if ( band > nbands(x) ) {
			warning("band too high. Set to nbands")
			band <- nbands(x) 
		}
		if ( band < 1) { 
			warning("band too low. Set to 1")
			band <- 1 
		}
		x@data@band <- as.integer(band)
		
		gd <- GDAL.open(filename)
		ct <- getColorTable( gd )
		if (! is.null(ct)) { x@legend@colortable <- ct }
		GDAL.close(gd)
	}

	
	if (fixGeoref) {
		cat('Fixing "AREA_OR_POINT=Point" georeference\n')
		rs <- res(x)
		xmin(x) <- xmin(x) - 0.5 * rs[1]
		xmax(x) <- xmax(x) - 0.5 * rs[1]
		ymin(x) <- ymin(x) + 0.5 * rs[2]
		ymax(x) <- ymax(x) + 0.5 * rs[2]
	}
	if (rotated) {
		x@rotated <- TRUE
		x@rotation <- rot
	}
	
	shortname <- gsub(" ", "_", ext(basename(filename), ""))
	x <- .enforceGoodLayerNames(x, shortname)
	x@file@name <- filename
	
	x@file@driver <- 'gdal' 
	projection(x) <- attr(gdalinfo, "projection")
	x@data@fromdisk <- TRUE
		
	datatype <- "FLT4S"
	minv = 	rep(Inf, nlayers(x))
	maxv = 	rep(-Inf, nlayers(x))
	if (type == 'RasterBrick') {
		try ( datatype <- .getRasterDType ( as.character( attr(gdalinfo, 'df')[1, 1]) ), silent=TRUE )
		try ( minv <- as.numeric( attr(gdalinfo, 'df')[, 2] ) , silent=TRUE ) 
		try ( maxv <- as.numeric( attr(gdalinfo, 'df')[, 3] ) , silent=TRUE ) 
		minv[minv == -4294967295] <- Inf
		maxv[maxv == 4294967295] <- -Inf
		minv <- as.vector(as.matrix(minv))
		maxv <- as.vector(as.matrix(maxv))
		if ( is.finite(minv) && is.finite(maxv) ) x@data@haveminmax <- TRUE 
		
	} else {
		try ( datatype <- .getRasterDType ( as.character( attr(gdalinfo, 'df')[band, 1]) ), silent=TRUE )
		minmax <- c(Inf, -Inf)
		try( minmax <- attr(gdalinfo, 'df')[band, 2:3] , silent=TRUE )
		if (all( minmax == c(-4294967295, 4294967295))) {
			minmax <- c(Inf, -Inf)
		}
		minv <- as.vector(as.matrix(minmax[1]))
		maxv <- as.vector(as.matrix(minmax[2]))
		if ( is.finite(minv) & is.finite(maxv) ) x@data@haveminmax <- TRUE 
	
	}
	x@file@datanotation <- datatype
	x@data@min <- minv
	x@data@max <- maxv

	RAT <- attr(gdalinfo, 'RATlist')
	if (! is.null(RAT)) {
		att <- vector(length=nlayers(x), mode='list')
		for (i in 1:length(RAT)) {
			if (! is.null(RAT[[i]])) {
				att[[i]] <- data.frame(RAT[[i]], stringsAsFactors=FALSE)
				
				if (! silent) {
					usage <- attr(RAT[[i]], 'GFT_usage')
					if (! isTRUE(usage[1] == "GFU_MinMax")) {
						warning('usage[1] != GFU_MinMax')
						# process min/max
					} else {
						if (! isTRUE(usage[2] == "GFU_PixelCount")) {
							warning('usage[2] != GFU_PixelCount')
						}
					}
				}
				x@data@isfactor[i] <- TRUE 
			}
		}
		x@data@attributes <- att
	}
	
#oblique.x   0  #oblique.y   0 
	return(x)
}

