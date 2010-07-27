# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date: June 2010
# Version 1.0
# Licence GPL v3


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
	if (type == "NC_CHAR" )  { return("INT1U") 
	} else if (type == "NC_BYTE" ) { return("INT1S")
	} else if (type == "NC_SHORT" ) { return("INT2S")
	} else if (type == "NC_INT" ) { return("INT4S")
	} else if (type == "NC_FLOAT" ) { return("FLT4S")
	} else if (type =="NC_DOUBLE" ) { return("FLT8S") 
	} else { return("FLT4S") }
}


.getNetCDFDType <- function(dtype) {
	if (!(dtype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S'))) {
		stop('not a valid data type')
	}
	type <- .shortDataType(dtype)
	size <- dataSize(dtype) * 8
	signed <- dataSigned(dtype)
	
	if (size == 8) {
		if (!signed) {
			return("NC_CHAR") #8-bit characters intended for representing text.
		} else {
			return("NC_BYTE")
		}
	} else if (type == 'INT') {
		if (!signed) {
			warning('netcdf only stores signed integers')
		}
		if (size == 16) { return( "NC_SHORT" ) 
		} else { return( "NC_INT" ) }
	} else {
		if (size == 32) { return( "NC_FLOAT" ) 
		} else {  return ( "NC_DOUBLE" )  }
	}
}


