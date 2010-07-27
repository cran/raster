# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : July 2010
# Version 1.0
# Licence GPL v3

setMethod('aggregate', signature(x='RasterLayer'), 
function(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename="", old=FALSE, ...)  {

	if (old) return(aggregate(stack(x), fact=fact, fun=fun, expand=expand, na.rm=TRUE, filename=filename, ...))

	if (length(fact)==1) {
		fact <- as.integer(round(fact))
		if (fact < 2) { stop('fact should be > 1') }
		xfact <- yfact <- fact
	} else if (length(fact)==2) {
		xfact <- as.integer(round(fact[[1]]))
		yfact <- as.integer(round(fact[[2]]))
		if (xfact < 2) { stop('fact[[1]] should be > 1') } 
		if (yfact < 2) { stop('fact[[2]] should be > 1') }
	} else {
		stop('length(fact) should be 1 or 2')
	}
	if (xfact > ncol(x)) {
		warning('aggregation factor is larger than the number of columns') 
		xfact <- ncol(x)
	}
	if (yfact > nrow(x)) {
		warning('aggregation factor is larger than the number of rows')
		yfact <- nrow(x)
	}

	if (expand) {
		rsteps <- as.integer(ceiling(nrow(x)/yfact))
		csteps <- as.integer(ceiling(ncol(x)/xfact))
	} else 	{
		rsteps <- as.integer(floor(nrow(x)/yfact))
		csteps <- as.integer(floor(ncol(x)/xfact))
	}
	
	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
		
	outRaster <- raster(x)	
	bndbox <- extent(xmin(x), xmx, ymn, ymax(x))
	extent(outRaster) <- bndbox
	rowcol(outRaster) <- c(rsteps, csteps) 

	if (class(fun) == 'character') {
		if (! fun %in% c('sum', 'mean') ) {
			stop("If 'fun' is a character variable, it should be either 'sum' or 'mean'")
		}
	}
	
	if (! fromDisk(x) & ! inMemory(x)) {
		return(outRaster)
	}	

	if (!expand) {
		lastcol <- csteps * yfact
		lastrow <- rsteps * xfact
	} else {
		lastcol <- x@ncols
		lastrow <- x@nrows
	}
	
	if (! canProcessInMemory(x)) {
		if (filename == '') { filename <- rasterTmpFile() }
	}
		
	if (filename == '') {
		v <- matrix(NA, ncol=nrow(outRaster), nrow=ncol(outRaster))
	} else {
		outRaster <- writeStart(outRaster, filename=filename, ...)
	}
		
  	pb <- pbCreate(rsteps, type=.progress(...))
		
		#vv <- matrix(ncol= csteps * yfact, nrow=rsteps * xfact)
	vv <- matrix(nrow= yfact * xfact, ncol=csteps)

	on.exit( options('warn'= getOption('warn')) )
	options('warn'=-1) 

	for (r in 1:rsteps) {
		
		startrow <- 1 + (r - 1) * yfact
		vals <- getValuesBlock(x, startrow, yfact, 1, lastcol)
			
			if (r==rsteps) { 
				endrow <- min(x@nrows, (startrow + yfact - 1))
				nrows <- endrow - startrow + 1
				vals = matrix(vals, nrow=nrows, byrow=TRUE )
				vv[] <- NA 
				vvv = vv[1:(nrows*yfact), ]
				vvv[1:length(vals)] <- vals
				vv[1:nrow(vvv),] <- vvv
		
			} else {
				vals = matrix(vals, nrow=yfact, byrow=TRUE )
				vv[1:length(vals)] = vals
			}
			
			if (is.character(fun)) {
				if (fun == 'mean') {
					vals <- colMeans(vv, na.rm=na.rm )
				} else {
					vals <- colSums(vv, na.rm=na.rm )			
				}
			} else {
				vals <- apply(vv, 2, fun, na.rm=na.rm )
			}
			if (filename == "") {
				v[, r] <- vals
			} else {
				outRaster <- writeValues(outRaster, vals, r)
			}
			pbStep(pb, r) 
	} 
		
	pbClose(pb)
	if (filename == "") { 
		outRaster <- setValues( outRaster, as.vector(v) )
	} else {
		outRaster <- writeStop(outRaster)
	}
	return(outRaster)
	
}
)
