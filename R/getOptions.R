# Author: Robert J. Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.9
# Licence GPL v3


showOptions <- function() {
	cat('format   :', .filetype(), '\n' )
	cat('datatype :', .datatype(), '\n')
	cat('overwrite:', .overwrite(), '\n')
	cat('progress :', .progress(), '\n')
	cat('chunksize:', .chunksize(), '\n')
	cat('tmpdir   :', .tmpdir(), '\n')
	if (.toDisk()) {
		cat('toDisk   : TRUE\n')
	}
}


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
	if (!file.exists(d)) {
		d <- paste(dirname(tempdir()), '/R_raster_tmp/', sep="")
		dir.create(d, showWarnings=FALSE )
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
		warning(datatype, 'is an invalid datatype, changed to FLT4S')
	}
	return(datatype)
}

.filetype <- function(..., format) {
	if (missing(format)) { 
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
				if (progress[1] %in% c('text', 'tcltk', 'windows')) {
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
			if (progress[1] %in% c('text', 'tcltk', 'windows')) {
				return(progress[1])
			} else {
				return('none')
			}
		} else {
			return('none')
		}
	}
}


.toDisk <- function(..., todisk) {
	if (missing(todisk)) { 
		todisk <- getOption('rasterToDisk')
		if (is.null(todisk)) {
			return(FALSE)  # the default
		} else {
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

