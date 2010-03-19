# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3


worldFile <- function(raster, extension=".wld") {
	hdrfile <- filename(raster)
	ext(hdrfile) <- extension
	thefile <- file(hdrfile, "w")  
	cat(xres(raster), "\n", file = thefile)
	cat("0\n", file = thefile)
	cat("0\n", file = thefile)
	cat(-1 * yres(raster), "\n", file = thefile)
    cat(xmin(raster) + 0.5 * xres(raster), "\n", file = thefile) 
    cat(ymax(raster) - 0.5 * yres(raster), "\n", file = thefile) 
	close(thefile)	
}
