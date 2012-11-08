# Author: Robert J. Hijmans
# Date: June 2010
# Version 1.0
# Licence GPL v3



.getCRSfromGridMap3 <- function(nc, gridmap) {
	m <- matrix(c("grid_mapping_name", "+proj", "false_easting", "+x_0","false_northing", "+y_0", "scale_factor_at_projection_origin", "+k_0", "scale_factor_at_central_meridian", "+k_0", "standard_parallel", "+lat_1", "standard_parallel1", "+lat_1", "standard_parallel2", "+lat_2", "longitude_of_central_meridian", "+lon_0", "longitude_of_projection_origin", "+lon_0", "latitude_of_projection_origin", "+lat_0", "straight_vertical_longitude_from_pole", "+lon_0"), ncol=2, byrow=TRUE)
	g <- list()
	for (i in 1:nrow(m)) {
		a <- att.get.ncdf(nc, gridmap, m[i,1])
		if (a$hasatt) {
			lst <- list(a$value)
			names(lst) <- m[i,1]
			g <- c(g, lst)
		}
	}
	.getCRSfromGridMap4(g)
}



.getCRSfromGridMap4 <- function(g) {
# based on info at 
# http://trac.osgeo.org/gdal/wiki/NetCDF_ProjectionTestingStatus
# accessed 7 October 2012
	prj <- matrix(c("albers_conical_equal_area", "aea", "azimuthal_equidistant", "aeqd", "lambert_cylindrical_equal_area", "cea", "lambert_azimuthal_equal_area", "laea", "lambert_conformal_conic", "lcc", "mercator", "merc", "orthographic", "ortho", "polar_stereographic", "stere", "stereographic", "stere", "transverse_mercator", "tmerc"), ncol=2, byrow=TRUE)
	
	m <- matrix(c("grid_mapping_name", "+proj", "false_easting", "+x_0","false_northing", "+y_0", "scale_factor_at_projection_origin", "+k_0", "scale_factor_at_central_meridian", "+k_0", "standard_parallel", "+lat_1", "standard_parallel1", "+lat_1", "standard_parallel2", "+lat_2", "longitude_of_central_meridian", "+lon_0", "longitude_of_projection_origin", "+lon_0", "latitude_of_projection_origin", "+lat_0", "straight_vertical_longitude_from_pole", "+lon_0"), ncol=2, byrow=TRUE)

	
	sp <- g$standard_parallel
	if (!is.null(sp)) {
		if (length(sp) > 1)
		g$standard_parallel1 <- sp[1]
		g$standard_parallel2 <- sp[2]
		g$standard_parallel <- NULL
	}
	vars <- names(g)
	vals <- unlist(g)
	i <- match(vars, m[,1])
	if (any(is.na(i))) {
		warning("could not process the CRS")
		print(as.matrix(g))
		return(NA)
	}
	tab <- cbind(m[i,], vals)
	j <- match(tab[1,3], prj[,1])
	tab[1,3] <- prj[j,2]
	paste(apply(tab[,2:3], 1, function(x)paste(x, collapse='=')), collapse=' ')
}




.NCDFversion4 <- function() {

	loadNCDF <- function() {
		if (!require(ncdf)) {
			stop('To open ncdf files, you need to first install package "ncdf" or "ncdf4"') 
		}
		options(rasterNCDF4 = FALSE)
		return(FALSE)
	}
	
	ncdf4 <- getOption('rasterNCDF4')

	if (is.null(ncdf4)) {
		if (length(find.package("ncdf4", quiet=TRUE)) > 0) {
			if (require(ncdf4, quietly=TRUE)) {
				options(rasterNCDF4 = TRUE)
				ncdf4 <- TRUE
				
			} else {
				ncdf4 <- loadNCDF()
			}
			
		} else {
			ncdf4 <- loadNCDF()
		}
	}
	return(ncdf4)
}



.isNetCDF <- function(x) {
	on.exit(options('warn'= getOption('warn')))
	options('warn'=-1) 
	fcon <- file(x, "rb")
	tst <- try( w <- readBin(fcon, what='character', n=1), silent=TRUE)
	close(fcon)
	if ( isTRUE((substr(w, 1, 3) == "CDF" ))) { return(TRUE) 
	} else { return(FALSE)
	}
}


.getRasterDTypeFromCDF <- function(type) { 
	if (type == "char" )  { return("INT1U") 
	} else if (type == "byte" ) { return("INT1S")
	} else if (type == "short" ) { return("INT2S")
	} else if (type == "int" ) { return("INT4S")
	} else if (type == "integer" ) { return("INT4S")
	} else if (type == "float" ) { return("FLT4S")
	} else if (type =="double" ) { return("FLT8S") 
	} else { return("FLT4S") }
}


.getNetCDFDType <- function(dtype) {
	if (!(dtype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S'))) {
		stop('not a valid data type')
	}
	type <- tolower(.shortDataType(dtype))
	size <- dataSize(dtype) * 8
	signed <- dataSigned(dtype)
	
	if (size == 8) {
		if (!signed) {
			return("char") #8-bit characters intended for representing text.
		} else {
			return("byte")
		}
	} else if (type == 'integer') {
		if (!signed) {
			warning('netcdf only stores signed integers')
		}
		if (size == 16) { 
			return( "short" ) 
		} else if (size == 32 ) { 
			return( "integer" ) 
		} else {
			return ( "double" )		
		}
	} else {
		if (size == 32) { 
			return( "float" ) 
		} else {  
			return ( "double" )  
		}
	}
}


