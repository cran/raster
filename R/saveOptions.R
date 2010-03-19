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

clearOptions <- function() {
	options(rasterDatatype = 'FLT4S')
	options(rasterFiletype = 'raster')
	options(rasterOverwrite = FALSE)
	options(rasterProgress = 'none')
	options(rasterTmpDir = .tmpdir())
	options(rasterFilename = '')
	options(rasterDataLocation = '')
		
	fn <- paste(R.home(component="etc"), '/', 'Rprofile.site', sep='')
	lst <- readLines(fn)
	lst <- .removeRasterOptions(lst)
	r <- try( write(unlist(lst), fn), silent = TRUE )
#	if (class(r) == "try-error") { cat('Cannot save options. No write access to: ', fn, '\n')	}
}



saveOptions <- function() {
	fn <- paste(R.home(component="etc"), '/', 'Rprofile.site', sep='')
	lst <- readLines(fn)
	lst <- .removeRasterOptions(lst)
	if (lst[length(lst)] != "") { lst <- c(lst, "") }
	lst <- c(lst, "# Options for the 'raster' package")
	lst <- c(lst, paste("options(rasterFiletype='", .filetype(), "')", sep='')) 
	lst <- c(lst, paste("options(rasterDatatype='", .datatype(), "')", sep=''))
	lst <- c(lst, paste("options(rasterOverwrite=", .overwrite(), ')', sep=''))
	lst <- c(lst, paste("options(rasterProgress='", .progress(), "')", sep=''))
	lst <- c(lst, paste("options(rasterTmpDir='", .tmpdir(), "')", sep=''))
	lst <- c(lst, paste("options(rasterDataDir='", .dataloc(), "')", sep=''))

	r <- try( write(unlist(lst), fn), silent = TRUE )
#	if (class(r) == "try-error") { cat('Cannot save options. No write access to: ', fn, '\n')	}
}
