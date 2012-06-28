# Author: Robert J. Hijmans
# September 2009
# Version 2.0
# Licence GPL v3


setOptions <- function(format, overwrite, datatype, tmpdir, tmptime, progress, timer, chunksize, maxmemory, todisk, setfileext, tolerance, standardnames) {
	
	setFiletype <- function(format) {
		if (.isSupportedFormat(format)) {	options(rasterFiletype = format)	
		} else { warning(paste('Cannot set filetype to unknown or unsupported file format:', format, '. See writeFormats()'))	}
	}
	
	setOverwrite <- function(overwrite) {
		if (is.logical(overwrite)) { options(rasterOverwrite = overwrite)
		} else {warning(paste('Could not set overwrite. It must be a logical value'))	}
	}
	
	setDataType <- function(datatype) {
		if (datatype %in% c('LOG1S', 'INT1S', 'INT2S', 'INT4S', 'INT4U', 'INT1U', 'INT2U', 'FLT4S', 'FLT8S')) {	
			options(rasterDatatype = datatype)
		} else { 
			warning(paste('Cannot set datatype to unknown type:',datatype))	
		}
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
	
	setTmpTime <- function(tmptime) {
		if (is.numeric(tmptime)) {
			if (tmptime > 1) {
				options(rasterTmpTime = tmptime)
			} else {
				warning(paste('Could not set tmptime. It must be > 1'))	
			}
		} else {
			warning(paste('Could not set tmptime. It must be a numerical value'))	
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
		chunksize <- max(1, round(chunksize[1]))
		chunksize <- min(chunksize, 10^9)
		options(rasterChunkSize = chunksize )
	}

	setFileExt <- function(setfileext) {
		options(rasterSetFileExt = as.logical(setfileext) )
	}

	setMaxMemorySize <- function(maxmemory) {
		maxmemory = max(10000, round(maxmemory[1]))
		options(rasterMaxMemory = maxmemory )
	}
	
	setTolerance <- function(x) {
		x <- max(0.000000001, min(x, 0.5))
		options(rasterTolerance = x)
	}
	
	setStandardNames <- function(x) {
		if (is.logical(x)) {
			if (is.na(x)) {
				x <- TRUE
			}
			options(rasterStandardNames = x)
		}
	}
		
 	
	if (!missing(format)) { setFiletype(format) }
	if (!missing(overwrite)) { setOverwrite(overwrite) }
	if (!missing(datatype)) { setDataType(datatype) }
	if (!missing(progress)) { setProgress(progress) }
	if (!missing(timer)) { setTimer(timer) }
	if (!missing(tmpdir)) { setTmpdir(tmpdir) }
	if (!missing(tmptime)) { setTmpTime(tmptime) }
	if (!missing(todisk)) { setToDisk(todisk) }
	if (!missing(setfileext)) { setFileExt(setfileext) }
	if (!missing(maxmemory)) { setMaxMemorySize(maxmemory) }
	if (!missing(chunksize)) { setChunksize(chunksize) }
	if (!missing(tolerance)) { setTolerance(tolerance) }
	if (!missing(standardnames)) { setStandardNames(standardnames) }
	
}

