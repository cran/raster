# Author: Robert J. Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.9
# Licence GPL v3


setOptions <- function(format, overwrite, datatype, tmpdir, progress, timer, chunksize, maxmemory, todisk) {
	
	setFiletype <- function(format) {
		if (.isSupportedFormat(format)) {	options(rasterFiletype = format)	
		} else { warning(paste('Cannot set filetype to unknown or unsupported file format:', format, '. See writeFormats()'))	}
	}
	
	setOverwrite <- function(overwrite) {
		if (is.logical(overwrite)) { options(rasterOverwrite = overwrite)
		} else {warning(paste('Could not set overwrite. It must be a logical value'))	}
	}
	
	setDataType <- function(datatype) {
		if (datatype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT8S', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S')) {	options(rasterDatatype = datatype)
		} else { warning(paste('Cannot set datatype to unknown type:',datatype))	}
	}
	
	setTmpdir <- function(tmpdir) {
		if (!missing(tmpdir)) {
			tmpdir <- trim(tmpdir)
			if (tmpdir != '') {
				lastchar = substr(tmpdir, nchar(tmpdir), nchar(tmpdir))
				if (lastchar != "/" & lastchar != '\\') {
					tmpdir <- paste(tmpdir, '/', sep='')
				}		
				res <- file.exists(substr(tmpdir, 1, nchar(tmpdir)-1))
				if (!res) { 
					res <- dir.create(tmpdir, recursive=TRUE, showWarnings = FALSE) 
				}
				if (res) { 
					options(rasterTmpDir = tmpdir) 
				} else { 
					warning(paste('could not create tmpdir:',tmpdir))	
				}
			}
		}
	}
	
	setProgress <- function(progress) {
		if (is.character(progress)) {
			progress <- tolower(trim(progress))
			if (progress %in% c('window', 'tcltk', 'windows')) { progress <- 'window' }
			if (! progress %in% c('text', 'window', '')) { 
				warning('invalid value for progress. Should be "window", "text", or ""')
			} else {
				options(rasterProgress = progress )
			}
		} else {
			warning('progress must be a character value')
		}
	}
	
	setTimer <- function(timer) {
		if (is.logical(timer)) {
			options(rasterTimer = timer )
		} else {
			warning(paste('timer must be a logical value'))	
		}
	}
	
	
	setToDisk <- function(todisk) {
		if (is.logical(todisk)) { 
			options(rasterToDisk = todisk )
		} else {
			warning(paste('todisk argument must be a logical value'))	
		}
	}
 
	setChunksize <- function(chunksize) {
		chunksize = max(1, round(chunksize[1]))
		options(rasterChunkSize = chunksize )
	}

	setMaxMemorySize <- function(maxmemory) {
		maxmemory = max(10000, round(maxmemory[1]))
		options(rasterMaxMemory = maxmemory )
	}
 	
	if (!missing(format)) { setFiletype(format) }
	if (!missing(overwrite)) { setOverwrite(overwrite) }
	if (!missing(datatype)) { setDataType(datatype) }
	if (!missing(progress)) { setProgress(progress) }
	if (!missing(timer)) { setTimer(timer) }
	if (!missing(tmpdir)) { setTmpdir(tmpdir) }
	if (!missing(todisk)) { setToDisk(todisk) }
	if (!missing(maxmemory)) { setMaxMemorySize(maxmemory) }
	if (!missing(chunksize)) { setChunksize(chunksize) }
}



#file <- function(filename) {
#	if (is.character(filename)) {
#		if (length(filename) == 1) {
#			options(rasterFilename = filename)
#		} else {
#			warning(paste('Could not set filename. It should not be a vector with length > 1'))	
#		}
#	} else {
#		warning(paste('Could not set filename. It should a character type variable'))	
#	}
#}
