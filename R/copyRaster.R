# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  February 2009
# Version 0.9
# Licence GPL v3


moveRasterFile <- function(raster, filename, overwrite=FALSE) {
	r <- copyRasterFile(raster, filename, overwrite)
	f <- trim(filename(raster))
	fileext <- toupper(ext(f)) 
	if (fileext == ".GRD") {
		file.remove(f)
		ext(f) <- '.gri'
		file.remove(f)
	}
	return(r)
}


copyRasterFile <- function(raster, filename, overwrite=FALSE) {
	f <- trim(filename(raster))
	fileext <- toupper(ext(f)) 
	if (fileext == ".GRD") {
		fgrd <- filename
		ext(fgrd) <- '.grd'
		fgri <- filename
		ext(fgri) <- '.gri'
		if ( file.exists(fgrd) & !overwrite ) {
			stop('file exists & overwrite = FALSE')
		}
		if ( file.exists(fgri) & !overwrite ) {
			stop('file exists & overwrite = FALSE')
		}
		res <- file.copy(f, fgrd, overwrite)
		if (!res) { stop('could not copy grd file') } 
		ext(f) <- ".gri"
		res <- file.copy(f, fgri, overwrite)
		if (!res) { stop('could not copy gri file') } 
		filename(raster) <- fgrd
		return(raster)
	} else {
		stop('only implemented for raster format files')
	}
}


