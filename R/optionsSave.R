# Author: Robert J. Hijmans, r.hijmans@gmail.com
# September 2009
# Version 0.9
# Licence GPL v3

.removeRasterOptions <- function(x) {
	y <- list()
	for (i in seq(along=x)) {
		if (!trim(x[[i]]) == "# Options for the 'raster' package" & !substr(trim(x[[i]]),1,14) == 'options(raster') {
			y <- c(y, x[[i]])
		}
	}
	return(y)
}



showOptions <- function() {
	cat('format    :', .filetype(), '\n' )
	cat('datatype  :', .datatype(), '\n')
	cat('overwrite :', .overwrite(), '\n')
	cat('progress  :', .progress(), '\n')
	cat('timer     :', .timer(), '\n')
	cat('chunksize :', .chunksize(), '\n')
	cat('maxmemory :', .maxmemory(), '\n')
	cat('tmpdir    :', .tmpdir(), '\n')
	cat('setfileext:', .setfileext(), '\n')
	cat('tolerance :', .tolerance(), '\n')
	
#	cat('usecluster:', .usecluster(), '\n')
	if (.toDisk()) {
		cat('toDisk    : TRUE\n')
	}
}

clearOptions <- function() {
	options(rasterFiletype = 'raster')
	options(rasterDatatype = 'FLT4S')
	options(rasterOverwrite = FALSE)
	options(rasterProgress = 'none')
	options(rasterTimer = FALSE)
	options(rasterChunkSize = 1000000)
	options(rasterMaxMemory = 10000000)
	options(setfileext = TRUE)
	options(rasterTmpDir = .tmpdir())
	options(rasterTolerance = 0.1)
		
	fn <- paste(R.home(component="etc"), '/', 'Rprofile.site', sep='')
	lst <- readLines(fn)
	lst <- .removeRasterOptions(lst)
	r <- try( write(unlist(lst), fn), silent = TRUE )
#	if (class(r) == "try-error") { cat('Cannot save options. No write access to: ', fn, '\n')	}
}



saveOptions <- function() {
	fn <- paste(R.home(component="etc"), '/', 'Rprofile.site', sep='')
	if (file.exists(fn)) {
		lst <- readLines(fn)
		if (length(lst) == 0) { lst <- "" }
		lst <- .removeRasterOptions(lst)
		if (lst[length(lst)] != "") { lst <- c(lst, "") }
	} else {
		lst <- ""
	}

	lst <- c(lst, "# Options for the 'raster' package")
	lst <- c(lst, paste("options(rasterFiletype='", .filetype(), "')", sep='')) 
	lst <- c(lst, paste("options(rasterDatatype='", .datatype(), "')", sep=''))
	lst <- c(lst, paste("options(rasterOverwrite=", .overwrite(), ')', sep=''))
	lst <- c(lst, paste("options(rasterProgress='", .progress(), "')", sep=''))
	lst <- c(lst, paste("options(rasterTimer=", .timer(), ')', sep=''))
	lst <- c(lst, paste("options(rasterChunkSize=", .chunksize(), ")", sep=''))
	lst <- c(lst, paste("options(rasterMaxMemory=", .maxmemory(), ")", sep=''))
	lst <- c(lst, paste("options(rasterSetFileExt=", .setfileext(), ')', sep=''))
	lst <- c(lst, paste("options(rasterTmpDir='", .tmpdir(), "')", sep=''))
	lst <- c(lst, paste("options(rasterTolerance=", .tolerance(), ')', sep=''))

	r <- try( write(unlist(lst), fn), silent = TRUE )
#	if (class(r) == "try-error") { cat('Cannot save options. No write access to: ', fn, '\n')	}
}
