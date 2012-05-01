# Author: Robert J. Hijmans
# Date : March 2011
# Version 1.0
# Licence GPL v3


.quickStack <- function(files, nbands=1, band=1, native=FALSE) {
	r <- raster(files[1], native=native)
	if (length(nbands) == 1) {
		nbands <- rep(nbands, length(files))
	} else {
		stopifnot(length(files == length(nbands)))
	}
	
	if (length(band) == 1) {
		band <- rep(band, length(files))
	} else {
		stopifnot(length(files == length(band)))
	}
	
	r@data@haveminmax=FALSE 
	r@file@nbands = nbands[1]
	r@data@band = band[1]
	
	ln <- extension(basename(files), '')
	s <- stack(r)
	s@layers <- sapply(1:length(files), 
			function(i){ 
				r@file@name <-  files[i]
				r@file@nbands <- nbands[i]
				r@data@band <-  band[i]
				r@layernames <- ln[i]
				r
			}
		)
	s@layernames <- ln
	s
}

