# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  May 2009
# Version 0.9
# Licence GPL v3



.fileDialog <- function(filetypes="{{All files} *}") {
	if (! require(tcltk) ) {
		stop('you need to install the tcltk library')
	}
	f <- tclvalue(tkgetSaveFile(filetypes=filetypes))
	return(f)
}


rasterTmpFile <- function()  {
	f <- getOption('rasterTmpFile')
	if (!is.null(f)) {
		f <- trim(f)
		if (! f == '' ) {
			options('rasterTmpFile' = NULL)
			return(f)
		}
	}
	
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


removeTmpFiles <- function(h=24) {
	
# remove files in the temp folder that are > h hours old	
	warnopt <- getOption('warn')
	on.exit(options('warn'= warnopt))

	d <- .removeTrailingSlash(.tmpdir())
	f <- list.files(path=d, pattern='raster_tmp*', full.names=TRUE)
	fin <- file.info(f)
	dif <- Sys.time() - fin$mtime
	dif <- as.numeric(dif, units="hours")
	
	dif[is.na(dif)] <- h + 1
	f <- f[dif > h]
	if (length(f) > 1) {
		file.remove(f)
	}
#	if (file.exists(d)) {
#		unlink(paste(d, "/raster_tmp_*", sep=""), recursive = FALSE)
#	}
	options('warn'=warnopt) 
}

showTmpFiles <- function() {
	d <- .removeTrailingSlash(.tmpdir())
	if (file.exists(d)) {
		f <- list.files(d, pattern='raster_tmp_')
		if (length(f) == 0) {
			cat('--- none ---\n')
		} else {
			extension(f) <- ''
			f <- unique(f)
			cat(f, "\n")
		}
	} else {
		cat('--- none ---\n')
	}
}

