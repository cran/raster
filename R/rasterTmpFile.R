# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  May 2009
# Version 0.9
# Licence GPL v3



rasterTmpFile <- function()  {
	extension <- .defaultExtension(.filetype())
	d <- .tmpdir()
	dir.create(d,  showWarnings = FALSE)
	f <- paste(round(runif(10)*10), collapse="")
	d <- paste(d, 'raster_tmp_', f, extension, sep="")
	if (getOption('verbose')) { cat('writing raster to:', d) }
	return(d)
}


.removeTrailingSlash <- function(d) {
		if (substr(d, nchar(d), nchar(d)) == '/') { d <- substr(d, 1, nchar(d)-1) }
		if (substr(d, nchar(d), nchar(d)) == '\\') { d <- substr(d, 1, nchar(d)-1) }
		return(d)
}


removeTmpFiles <- function() {
	d <- .removeTrailingSlash(.tmpdir())
	if (file.exists(d)) {
		unlink(paste(d, "/raster_tmp_*", sep=""), recursive = FALSE)
	}
}

showTmpFiles <- function() {
	d <- .removeTrailingSlash(.tmpdir())
	if (file.exists(d)) {
		f <- list.files(d, pattern='raster_tmp_')
		if (length(f) == 0) {
			cat('--- none ---\n')
		} else {
			ext(f) <- ''
			f <- unique(f)
			cat(f, "\n")
		}
	} else {
		cat('--- none ---\n')
	}
}

