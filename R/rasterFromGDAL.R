# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3

.gdFixGeoref <- function(mdata) {
	gdversion = getGDALVersionInfo()
	gdversion = trim(substr(gdversion, 5, 10))
	test <- gdversion < '1.8.0'	
	if (test) {
		if (! is.null(mdata) ) {
			for (i in 1:length(mdata)) {
				if (mdata[i] == "AREA_OR_POINT=Area") {
					return(FALSE)
				} else if (mdata[i] == "AREA_OR_POINT=Point") {
					return(TRUE)
				}
			}
		}
	}
	return(FALSE)
}



.rasterFromGDAL <- function(filename, band, type, RAT=FALSE, silent=TRUE, ...) {	

# most of this was taken from the GDALinfo function in rgdal

	if (! .requireRgdal() ) { 
		stop('package rgdal is not available') 
	}

	# suppressing the geoTransform warning...
	w <- getOption('warn')
	on.exit(options('warn'= w))
	options('warn'= -1) 
	
	options('warn'= w) 
	x <- GDAL.open(filename, silent=TRUE)
	
	nc <- as.integer(.Call('RGDAL_GetRasterXSize', x, PACKAGE="rgdal"))
	nr <- as.integer(.Call('RGDAL_GetRasterYSize', x, PACKAGE="rgdal"))
    nbands <- as.integer(.Call('RGDAL_GetRasterCount', x, PACKAGE="rgdal"))

    #dr <- getDriverName(getDriver(x))
    gt <- .Call("RGDAL_GetGeoTransform", x, PACKAGE = "rgdal")
    if (attr(gt, "CE_Failure") ) {
		warning("GeoTransform values not available; georeferencing will not be correct")
	}

    ysign <- sign(gt[6])
    offset.y <- ifelse(ysign < 0, gt[4] + ysign * nr * abs(gt[6]), gt[4] + abs(gt[6]))
	
	xn <- round(gt[1], digits=9)
	xx <- xn + abs(gt[2]) * nc
	xx <- round(xx, digits=9)
	yn <- round(offset.y, digits=9)
	yx <- yn + abs(gt[6]) * nr
	yx <- round(yx, digits=9)

	rotated <- FALSE
	if (abs(gt[3]) != 0 | abs(gt[5]) != 0) {
		rotated <- TRUE

		## adapted from rgdal::getGeoTransFunc
		warning('\n\n This file has a rotation\n Support such files is limited and results of data processing might be wrong.\n Proceed with caution & consider using the "rectify" function\n')

		rotMat <- matrix(gt[c(2, 3, 5, 6)], 2)
		invMat <- solve(rotMat)
		
		offset <- gt[c(1, 4)]
		trans <- function(x, inv=FALSE) {
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
	
		crd <- trans(cbind(c(0, 0, nc, nc), c(0, nr, 0, nr))+0.5)
		rot <- new(".Rotation")
		rot@geotrans <- as.vector(gt)
		rot@transfun <- trans

		xn  <- min(crd[,1])
		xx  <- max(crd[,1])
		yn  <- min(crd[,2])
		yx  <- max(crd[,2])
		
	} 
	
	mdata <- .Call("RGDAL_GetMetadata", x, NULL, PACKAGE = "rgdal")
	fixGeoref <- FALSE
	try( fixGeoref <- .gdFixGeoref(mdata), silent=TRUE )

	if (type == 'RasterBrick') {
	
		r <- brick(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs="")
		r@file@nbands <- r@data@nlayers <- nbands
		band <- 1:nbands
		
	} else {
	
		r <- raster(ncols=nc, nrows=nr, xmn=xn, ymn=yn, xmx=xx, ymx=yx, crs="")
		r@file@nbands <- as.integer(nbands)
		band <- as.integer(band)
		if ( band > nbands(r) ) {
			warning("band too high. Set to nbands")
			band <- nbands(r) 
		}
		if ( band < 1) { 
			warning("band too low. Set to 1")
			band <- 1 
		}
		r@data@band <- as.integer(band)
		ct <- getColorTable( x )
		if (! is.null(ct)) { r@legend@colortable <- ct }
		nbands <-1 
	}
	if (rotated) {
		r@rotated <- TRUE
		r@rotation <- rot
	}	

	projection(r) <- .Call("RGDAL_GetProjectionRef", x, PACKAGE = "rgdal")

   	r@history[[1]] <- mdata
    r@history[[2]] <- .Call("RGDAL_GetMetadata", x, "SUBDATASETS", PACKAGE = "rgdal")
	
    GDType <- character(nbands)
    Bmin <- Bmax <- numeric(nbands)
	hasNoDataValues <- logical(nbands)
	NoDataValues <- numeric(nbands)
	RATlist <- vector(mode = "list", length = nbands)
	CATlist <- vector(mode = "list", length = nbands)
	blockrows <- integer(nbands)
	blockcols <- integer(nbands)
	for (i in 1:nbands) {
		rstband <- getRasterBand(x, band[i])
		GDType[i] <- rgdal:::.GDALDataTypes[(.Call("RGDAL_GetBandType", rstband, PACKAGE = "rgdal")) + 1]
		statsi <- .Call("RGDAL_GetBandStatistics", rstband, silent, PACKAGE = "rgdal")
		if (is.null(statsi)) {
			Bmin[i] <- .Call("RGDAL_GetBandMinimum", rstband, PACKAGE = "rgdal")
			Bmax[i] <- .Call("RGDAL_GetBandMaximum", rstband, PACKAGE = "rgdal")
		} else {
			Bmin[i] <- statsi[1]
			Bmax[i] <- statsi[2]
			#Bmn[i] <- statsi[3]
			#Bsd[i] <- statsi[4]
		}
		if (RAT) {
			RATi <- .Call("RGDAL_GetRAT", rstband, PACKAGE = "rgdal")
			if (!is.null(RATi)) {
				RATlist[[i]] <- RATi
			}

			CATi <- .Call("RGDAL_GetCategoryNames", rstband, PACKAGE = "rgdal")
			if (!is.null(CATi)) {
				CATlist[[i]] <- CATi
			}
			
		}
		NDV <- .Call("RGDAL_GetBandNoDataValue", rstband, PACKAGE = "rgdal")
		if (is.null(NDV)) {
			hasNoDataValues[i] <- FALSE
		} else {
			hasNoDataValues[i] <- TRUE
			NoDataValues[i] <- NDV[1]
		}
		
		bs <- getRasterBlockSize( getRasterBand(x, i) )
		blockrows[i] <- bs[1]
		blockcols[i] <- bs[2]
	}

	GDAL.close(x)

	r@file@blockrows <- blockrows
	r@file@blockcols <- blockcols

	if (fixGeoref) {
		cat('Fixing "AREA_OR_POINT=Point" georeference\n')
		rs <- res(r)
		xmin(r) <- xmin(r) - 0.5 * rs[1]
		xmax(r) <- xmax(r) - 0.5 * rs[1]
		ymin(r) <- ymin(r) + 0.5 * rs[2]
		ymax(r) <- ymax(r) + 0.5 * rs[2]
	}
	
	if (type == 'RasterBrick') {
		layerNames(r) <- rep(gsub(" ", "_", extension(basename(filename), "")), nbands)
	} else {
		lnames <- gsub(" ", "_", extension(basename(filename), ""))
		if (nbands > 1) {
			lnames <- paste(lnames, '_', band, sep='')
		}
		layerNames(r) <- lnames
		
	}
	r@file@name <- filename
	r@file@driver <- 'gdal' 
 

	r@data@fromdisk <- TRUE
		
	datatype <- "FLT4S"
	minv = 	rep(Inf, nlayers(r))
	maxv = 	rep(-Inf, nlayers(r))
	try ( minv <- as.numeric( Bmin ) , silent=TRUE ) 
	try ( maxv <- as.numeric( Bmax ) , silent=TRUE ) 
	minv[minv == -4294967295] <- Inf
	maxv[maxv == 4294967295] <- -Inf
	try ( datatype <- .getRasterDType ( GDType[1] ), silent=TRUE )
	
	if ( is.finite(minv) && is.finite(maxv) ) r@data@haveminmax <- TRUE 
	r@file@datanotation <- datatype
	r@data@min <- minv
	r@data@max <- maxv

	if (! is.null(RATlist[[1]])) {
		att <- vector(length=nlayers(r), mode='list')
		for (i in 1:length(RATlist)) {
			if (! is.null(RATlist[[i]])) {
				att[[i]] <- data.frame(RATlist[[i]], stringsAsFactors=FALSE)
				
				if (! silent) {
					usage <- attr(RATlist[[i]], 'GFT_usage')
					if (! isTRUE(usage[1] == "GFU_MinMax")) {
						warning('usage[1] != GFU_MinMax')
						# process min/max
					} else {
						if (! isTRUE(usage[2] == "GFU_PixelCount")) {
							warning('usage[2] != GFU_PixelCount')
						}
					}
				}
				r@data@isfactor[i] <- TRUE 
			}
		}
		r@data@attributes <- att
	}
	
#oblique.x   0  #oblique.y   0 
	return(r)
}

