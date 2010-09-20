# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2008
# Version 0.9
# Licence GPL v3


.rasterFromGDAL <- function(filename, band, type) {	
	if (! .requireRgdal() ) { stop('package rgdal is not available') }

	# suppressing the geoTransform warning...
	w <- getOption('warn')
	on.exit(options('warn'= w))
	options('warn'=-1) 
	gdalinfo <- GDALinfo(filename, silent=TRUE)
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
		if ( is.finite(minv) && is.finite(maxv) ) x@data@haveminmax <- TRUE 
	} else {
		try ( datatype <- .getRasterDType ( as.character( attr(gdalinfo, 'df')[band, 1]) ), silent=TRUE )
		minmax <- c(Inf, -Inf)
		try( minmax <- attr(gdalinfo, 'df')[band, 2:3] , silent=TRUE )
		if (all( minmax == c(-4294967295, 4294967295))) {
			minmax <- c(Inf, -Inf)
		}
		minv <- minmax[1]
		maxv <- minmax[2]
		if ( is.finite(minv) & is.finite(maxv) ) x@data@haveminmax <- TRUE 
	}
	
	dataType(x) <- datatype
	x@data@min <- minv 
	x@data@max <- maxv
	
#oblique.x   0  #oblique.y   0 
	return(x)
}

