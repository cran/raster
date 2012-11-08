

.requireRgdal <- function(stopIfAbsent=TRUE) {
	
	y <- getOption('rasterGDALLoaded')
	
	w <- getOption('warn')
	options('warn'=-1) 
	x <- isTRUE( try( require(rgdal, quietly=TRUE ) ) )
	options('warn'= w) 
	
	if (! isTRUE(y) ) {
		if (x) {
			options('rasterGDALLoaded'=TRUE)
			gdversion <- getGDALVersionInfo()
			gdversion <- trim(substr(gdversion, 5, 10))
			options('rasterGDALVersion'=gdversion)
			pkg.info <- utils::packageDescription('rgdal') 
			test <- utils::compareVersion(pkg.info[["Version"]], "0.7-19") > 0
			options('rasterNewRGDALVersion' = test)
		} else if (stopIfAbsent) {
			stop("package 'rgdal' is not available")
		}
	}
	return(x)
}

# temporary tricks to move avoid .Calling rgdal as per CRAN request
# next version of raster should depend on rgdal > "0.7-12"

.gd_SetNoDataValue <- function(object, NAflag) {
	if (isTRUE(getOption('rasterNewRGDALVersion'))) {
		rgdal:::.gd_SetNoDataValue(object, NAflag)
	} else {
		.Call("RGDAL_SetNoDataValue", object, as.double(NAflag), PACKAGE="rgdal")
	}
}


.gd_SetGeoTransform <- function(object, geotrans) {
	if (getOption('rasterNewRGDALVersion')) {
		rgdal:::.gd_SetGeoTransform(object, geotrans)
	} else {
		.Call("RGDAL_SetGeoTransform", object, geotrans, PACKAGE="rgdal")
	}
}


.gd_SetProject <- function(object, proj4string) {
	if (getOption('rasterNewRGDALVersion')) {
		rgdal:::.gd_SetProject(object, proj4string)	
	} else {
		.Call("RGDAL_SetProject", object, proj4string, PACKAGE="rgdal")
	}
}


.gd_SetStatistics <- function(object, statistics) {
	if (getOption('rasterNewRGDALVersion')) {
		rgdal:::.gd_SetStatistics(object, statistics)
	} else {
		.Call("RGDAL_SetStatistics", object, as.double(statistics), PACKAGE="rgdal")
	}
}

.gd_transform <- function(projfrom, projto, n, x, y) {
	if (getOption('rasterNewRGDALVersion')) {
		rgdal:::.gd_transform(projfrom, projto, n, x, y)
	} else {
		.Call("transform", projfrom, projto, n, x, y, PACKAGE="rgdal")
	}
}


# gdalinfo: colortables, blocksizes