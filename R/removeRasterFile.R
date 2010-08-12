# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  February 2009
# Version 0.9
# Licence GPL v3


removeRasterFile <- function(raster) {
	if ( inherits(raster, 'Raster')) {
		if ( inherits(raster, 'RasterStack')) {
			stop('cannot do this with a RasterStack')
		}
		fname <- filename(raster)
#  need to close the connection in the object in the Global environement; not here	 ????
#		raster <- closeConnection(raster)
		filename(raster) <- ''
	} else {
		fname <- trim(raster)
	}
	fileext <- toupper(ext(fname))
    if (fileext == ".GRD") {
		fgrd <- fname
        ext(fgrd) <- '.grd'
        fgri <- fname
		ext(fgri) <- '.gri'
		if (!file.exists(fgrd) | !file.exists(fgri)) {
			stop('file does not exist')
		}
		res <- file.remove(fgrd)
		if (!res) { stop('could not remove grd file') }
		res <- file.remove(fgri)
        if (!res) { stop('could not remove gri file') }
    } else {
		stop('only implemented for raster format files')
    }
	if (inherits(raster, 'RasterLayer')) {
		return(raster)
	} else {
		return('OK')
	}
}
