# Author: Robert J. Hijmans
# Date : March 2011
# Version 1.0
# Licence GPL v3


.quickStack <- function(files) {
	r <- raster(files[1])
	r@data@haveminmax=FALSE 
	ln <- extension(basename(files), '')
	s <- stack(r)
	s@layers <- sapply(1:length(files), 
			function(i){ 
				r@file@name <-  files[i]
				r@layernames <- ln[i]
				r
			}
		)
	s@layernames <- ln
	s
}

