# Author: Robert J. Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.9
# Licence GPL v3


.dataloc <- function(){
	d <- getOption('rasterDataDir')
	if (is.null(d) ) {
		d <- getwd()
	} else {
		d <- trim(d)
		if (d=='') {
			d <- getwd()
		}
	}
	return(d)
}	


.tmpdir <- function() {
	d <- getOption('rasterTmpDir')
	if (is.null(d)) {
		d <- paste(dirname(tempdir()), '/R_raster_tmp/', sep="")
	}
	lastchar = substr(d, nchar(d), nchar(d))
	if (lastchar == "/" | lastchar == '\\') {
		dd <- substr(d, 1, nchar(d)-1)
	}		
	if (!file.exists(dd)) {
		d <- paste(dirname(tempdir()), '/R_raster_tmp/', sep="")
		dir.create(d, recursive=TRUE, showWarnings=FALSE )
	}
	return(d)
}



.chunksize <- function(){
	d <- getOption('rasterChunkSize')
	if (is.null(d)) {
		return( 1000000 )
	} 
	d <- round(as.numeric(d[1]))
	if (is.na(d)) {
		d <- 1000000
	} 
	if (d < 1) {
		d <- 1000000
	} 
	d <- max(d, 10000)
	return(d)
}	



.setfileext <- function() {
	d <- getOption('rasterSetFileExt')
	if (is.null(d)) {
		return( TRUE )
	} 
	return(as.logical(d))
}	


.maxmemory <- function() {
	default <- 100000000
	d <- getOption('rasterMaxMemory')
	if (is.null(d)) {
		return( default )
	} 
	d <- round(as.numeric(d[1]))
	if (is.na(d)) {
		d <- default
	} 
	if (d < 1) {
		d <- default
	} 
	d <- max(d, 10000)
	return(d)
}


.overwrite <- function(..., overwrite) {
	if (missing(overwrite)) { 
		overwrite <- getOption('rasterOverwrite')
		if (is.null(overwrite)) {
			return(FALSE)
		} else {
			if (is.logical(overwrite)) {
				return(overwrite)
			} else {
				return(FALSE)
			}
		}
	} else { 
		if (is.logical(overwrite)) {
			return(overwrite)
		} else {
			return(FALSE)
		}
	}
}


.datatype <- function(..., datatype) {
	if (missing(datatype)) { 
		datatype <- getOption('rasterDatatype')
		if (is.null(datatype)) {
			return('FLT4S') 
		} 
	} 
	if (! datatype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S')) {
		warning(datatype, ' is an invalid datatype, changed to FLT4S')
	}
	return(datatype)
}

.getFormat <- function(filename) {
	ext <- tolower(ext(filename, maxchar=5))
	if (nchar(ext) < 3) {
		return('')
	} else {
		if (ext == '.tif' | ext == '.tiff') { return('GTiff')
		} else if (ext == '.grd') { return('raster')
		} else if (ext == '.asc') { return('ascii')
		} else if (ext == '.nc') { return('CDF')
		} else if (ext == '.ncdf') { return('CDF')
		} else if (ext == '.sgrd') { return('SAGA')
		} else if (ext == '.sdat') { return('SAGA')
		} else if (ext == '.bil') { return('BIL')
		} else if (ext == '.bsq') { return('BSQ')
		} else if (ext == '.bip') { return('BIP')
		} else if (ext == '.bmp') { return('BMP') 
		} else if (ext == '.gen') { return('ADRG') 
		} else if (ext == '.bt') { return('BT') 
		} else if (ext == '.envi') { return('ENVI')
		} else if (ext == '.ers') { return('ERS') 
		} else if (ext == '.img') { return( 'HFA') 
		} else if (ext == '.rst') { return('RST') 
		} else if (ext == '.mpr') { return('ILWIS')
		} else if (ext == '.rsw') { return('RMF')
		} else { 
			warning('extension ', ext, ' is unknown. Using default format.')
			return('') 
		}
	}
	
}


.filetype <- function(..., format, filename='') {
	if (missing(format)) { 
		format <- .getFormat(filename)
		if (format != '') {
			return(format)
		}
		
		format <- getOption('rasterFiletype')
		if (is.null(format)) {
			return('raster') 
		} else {
			return(format)
		}
		
	} else { 
		return(format)
	}
}

.progress <- function(..., progress) {
	if (missing(progress)) { 
		progress <- getOption('rasterProgress')
		if (is.null(progress)) {
			return('none') 
		} else {
			if (is.character(progress)) {
				if (progress[1] %in% c('text', 'window', 'tcltk', 'windows')) {
					return(progress[1])
				} else {
					return('none')
				}
			} else {
				return('none')
			}
		}
	} else { 
		if (is.character(progress)) {
			if (progress[1] %in% c('text', 'window', 'tcltk', 'windows')) {
				return(progress[1])
			} else {
				return('none')
			}
		} else {
			return('none')
		}
	}
}


.timer <- function(..., timer) {
	if (missing(timer)) { 
		timer <- getOption('rasterTimer')
		if (is.null(timer)) {
			return(FALSE) 
		} else {
			return( as.logical(timer) )
		}
	} else {
		return(as.logical(timer))
	}
}	
	

.toDisk <- function(..., todisk) {
	if (missing(todisk)) { 
		todisk <- getOption('rasterToDisk')
		if (is.null(todisk)) {
			return(FALSE)  # the default
		} else {
			try (todisk <- as.logical(todisk))
			if (is.logical(todisk)) {
				return(todisk)
			} else {
				return(FALSE)
			}
		}
	} else { 
		if (is.logical(todisk)) {
			return(todisk)
		} else {
			return(FALSE)
		}
	}
}

.usecluster <- function(...) {
	usecluster <- list(...)$usecluster
	if (is.null(usecluster)) { 
		usecluster <- getOption('rasterUseCluster')
		if (is.null(usecluster)) {
			return(FALSE)  # the default
		} else {
			try (usecluster <- as.logical(usecluster), silent=TRUE)
			if (isTRUE(usecluster)) {
				return(TRUE)
			} else {
				return(FALSE)
			}
		}
	} else { 
		if (is.logical(usecluster)) {
			return(usecluster)
		} else {
			return(FALSE)
		}
	}
}

