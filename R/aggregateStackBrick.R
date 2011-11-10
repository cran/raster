# Authors: Robert J. Hijmans and Jacob van Etten
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

setMethod('aggregate', signature(x='RasterStackBrick'), 
function(x, fact=2, fun=mean, expand=TRUE, na.rm=TRUE, filename="", ...)  {

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
	if (xfact > ncol(x)) { warning('aggregation factor is larger than the number of columns') }
	if (yfact > nrow(x)) { warning('aggregation factor is larger than the number of rows')}

	if (expand) {
		rsteps <- as.integer(ceiling(nrow(x)/yfact))
		csteps <- as.integer(ceiling(ncol(x)/xfact))
	} else 	{
		rsteps <- as.integer(floor(nrow(x)/yfact))
		csteps <- as.integer(floor(ncol(x)/xfact))
	}
	
	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
		
	outRaster <- brick(x, values=FALSE)

	extent(outRaster) <- extent(xmin(x), xmx, ymn, ymax(x))
	dim(outRaster) <- c(rsteps, csteps) 
	layerNames(outRaster) <- layerNames(x)
	

	if (! inherits(x, 'RasterStack' )) {
		if ( ! fromDisk(x)  & ! inMemory(x) ) {
			return(outRaster)
		}
	}	

	if (canProcessInMemory(x, nlayers(x)+2)) {
	
		xx <- raster(x)		
		x <- getValues(x)
		cols <- rep(rep(1:csteps, each=xfact)[1:ncol(xx)], times=nrow(xx))
		rows <- rep(1:rsteps, each=ncol(xx) * yfact)[1:ncell(xx)]
		cells <- cellFromRowCol(xx, rows, cols)
		
		x <- as.matrix( aggregate(x, list(cells), fun, na.rm=na.rm ))[,-1]
		rm(cells)
		
		x <- setValues(outRaster, x)
		if (filename != "") {
			x <- writeRaster(x, filename=filename, ...)
		}
		return(x)

	} else  { 
	
		if (filename == '') { filename <- rasterTmpFile() }
		
		cols <- rep(rep(1:csteps,each=xfact)[1:ncol(x)], times=yfact)
		rows <- rep(1, each=(ncol(x) * yfact))
		
		outRaster <- writeStart(outRaster, filename=filename, ...)
		
		cells <- cellFromRowCol(x, rows, cols)
		nrows = yfact

		w <- getOption('warn')
		on.exit(options('warn' = w))
		options('warn'=-1) 
		
		pb <- pbCreate(rsteps, ...)
		for (r in 1:rsteps) {
			startrow <- 1 + (r - 1) * yfact
			if ( r==rsteps) {
				endrow <- min(nrow(x), startrow + yfact - 1)
				nrows <- endrow - startrow + 1
				theserows <- (startrow * rows)[1:(ncol(x)*nrows)]
				cols <- cols[1:(ncol(x)*nrows)]
				cells <- cellFromRowCol(x, theserows, cols)
			}	
			vals <- getValues(x, startrow, nrows)
			vals <- as.matrix( aggregate(vals, list(cells), fun, na.rm=na.rm ))[,-1]
		
			outRaster <- writeValues(outRaster, vals, r)
			pbStep(pb, r) 
		} 
		pbClose(pb)
		outRaster <- writeStop(outRaster)
		return(outRaster)
	}
}
)

