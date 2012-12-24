# Author: Robert J. Hijmans
# Date : September 2012
# Version 1.0
# Licence GPL v3



.requireRgdal <- function(stopIfAbsent=TRUE) {
	
	y <- getOption('rasterGDALLoaded')
	
	w <- getOption('warn')
	options('warn'=-1) 
	x <- isTRUE( try( require(rgdal, quietly=TRUE ) ) )
	options('warn'= w) 
	
	if (! isTRUE(y) ) {
		if (x) {
			gdversion <- getGDALVersionInfo()
			gdversion <- trim(substr(gdversion, 5, 10))
			pkg.info <- utils::packageDescription('rgdal') 
			test <- utils::compareVersion(pkg.info[["Version"]], "0.7-21") > 0
			if (!test) {
				stop('you use rgdal version: ', pkg.info[["Version"]], '\nYou need version 0.7-22 or higher')
			}
			options('rasterGDALLoaded'=TRUE)
			options('rasterGDALVersion'=gdversion)
			options('rasterNewRGDALVersion' = test)
		} else if (stopIfAbsent) {
			stop("package 'rgdal' is not available")
		}
	}
	return(x)
}

# temporary tricks to avoid .Calling rgdal as per CRAN request

.gd_SetNoDataValue <- function(object, NAflag) {
	if (isTRUE(getOption('rasterNewRGDALVersion'))) {
		rgdal:::.gd_SetNoDataValue(object, NAflag)
	} else {
		stop("you need a newer version of rgdal to use this function")
	}
}


.gd_SetGeoTransform <- function(object, geotrans) {
	if (getOption('rasterNewRGDALVersion')) {
		rgdal:::.gd_SetGeoTransform(object, geotrans)
	} else {
		stop("you need a newer version of rgdal to use this function")
	}
}


.gd_SetProject <- function(object, proj4string) {
	if (getOption('rasterNewRGDALVersion')) {
		rgdal:::.gd_SetProject(object, proj4string)	
	} else {
		stop("you need a newer version of rgdal to use this function")
	}
}


.gd_SetStatistics <- function(object, statistics) {
	if (getOption('rasterNewRGDALVersion')) {
		rgdal:::.gd_SetStatistics(object, statistics)
	} else {
		stop("you need a newer version of rgdal to use this function")
	}
}

.gd_transform <- function(projfrom, projto, n, x, y) {
	if (getOption('rasterNewRGDALVersion')) {
		rgdal:::.gd_transform(projfrom, projto, n, x, y)
	} else {
		stop("you need a newer version of rgdal to use this function")
	}
}


# gdalinfo: colortables, blocksizes