# Robert J. Hijmans
# May 2010
# Version 1.0
# Licence GPL v3


rectify <- function(x, ext, res, method='ngb', filename='', ...) {
	stopifnot(x@rotated)
	r <- x@rotation
	if (!missing(ext)) {
		ext <- extent(ext)
	} else {
		ext <- rbind(r@upperleft, r@lowerleft, r@upperright, r@lowerright) 
		ext <- extent(as.vector(apply(ext, 2, range)))
	}
	out <- raster(ext)
	if (! missing(res)) {
		res(out) <- res
	} else {
		res(out) <- r@geotrans[c(3,5)]
	}
	resample(x, out, method=method, filename=filename, ...)
}

