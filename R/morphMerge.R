# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  September 2010
# Version 1,0
# Licence GPL v3

.morphMerge <- function(x, y, ..., crs, res, method='bilinear', filename='') {

	warning('this function is still experimental, please provide feedback on odd behavior')
	
	if (missing(res)) { stop('provide a res a./rugment') }
	if (missing(crs)) { stop('provide a crs argument') }
	if ( projection(crs) == 'NA' ) { stop('crs cannot be NA') }
	validObject(projection(crs, asText=FALSE))
	
	datatype <- .datatype(...)
	overwrite <- .overwrite(...)
	filetype <- .filetype(...)	
	
	rl <- .makeRasterList(x, y, list(...), unstack=FALSE)
	es <- sapply( rl, extent )
	ep <- sapply( es, function(x) projectExtent(x, crs) )
	e <- unionExtent(unlist(ep)) + max(res)
	r <- raster(e, crs=crs)
	res(r) <- res
	for (i in 1:length(rl)) {
		ex <- ep[[i]] + max(res)
		rc <- crop(r, ex) 
		if ( projection(crs) != projection(rl[[i]]) ) {
			rl[[i]] <- projectRaster( rl[[i]], rc, method=method )
		} else {
			rl[[i]] <- resample(rl[[i]], rc, method=method)
		}
	}
	r <- cover(rl, filename=filename, format=filetype, datatype=datatype, overwrite=overwrite)
}


