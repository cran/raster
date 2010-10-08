# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2010
# Version 1.0
# Licence GPL v3


.morph <- function(from, to, method='bilinear', filename='', ...) {
	
	warning('this function is still experimental, please provide feedback on odd behavior')
	
	projfrom <- projection(from)
	projto <- projection(to)
	if (projfrom == "NA") { stop("input projection is NA") }
	if (projto == "NA") { stop("output projection is NA") }
	if (!identical(projfrom, projto)) {
		return(projectRaster(from, to, ...))
	} 
	
	if ( as(from, 'BasicRaster') == as(to, 'BasicRaster') ) {
		warning('nothing to do; returning "from"')
		return(from)
	} 
	if (! isTRUE( all.equal(origin(from), origin(to)) )) {
		return( resample(from, to, method=method, filename=filename, ...  ) )
	}	
	
	if (isTRUE( all.equal(res(from), res(to)) )) {
		# same origin and resolution, use crop and/or expand
		efrom = extent(from)
		eto = extent(to)
		eunion = unionExtent(efrom, eto)
		if (identical(eunion, eto)) {
			return(expand(efrom, eto, filename=filename, ...))
		} else {
			r <- crop(efrom, eto, filename=filename, ...)
			if (!identical(extent(r), eto)) {
				r <- expand(r, eto, filename=filename, ...)
			}
			return(r)
		}
		
	} else {
		# can we (dis)aggregate
		resfromx <- res(from)[1]
		restox <- res(to)[1]
		resfromy <- res(from)[2]
		restoy <- res(to)[2]
		if (resfromx == restox & resfromy == restoy) {
			if (resfromx %% restox == 0) {
				if (resfromx/restox > 1) {
					return(disaggregate(from, to, resfromx/restox, filename=filename, ...))
				} else {
					return(aggregate(from, to, resfromx/restox, filename=filename, ...))
				} 
			}
		} else {
			# needs more work, resample for now
			return( resample(from, to, method=method, filename=filename, ...  ) )
		}
	} 
	return( resample(from, to, method=method, filename=filename, ...  ) )
}

