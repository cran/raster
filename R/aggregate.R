# Authors: Robert J. Hijmans and Jacob van Etten
# contact: r.hijmans@gmail.com
# Date : October 2008
# Version 0.9
# Licence GPL v3

setMethod('aggregate', signature(x='RasterLayer'), 

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
	if (xfact > ncol(x)) {warning('aggregation factor is larger than the number of columns') }
	if (yfact > nrow(x)) {warning('aggregation factor is larger than the number of rows')}

	if (expand) {
		rsteps <- as.integer(ceiling(nrow(x)/yfact))
		csteps <- as.integer(ceiling(ncol(x)/xfact))
	} else 	{
		rsteps <- as.integer(floor(nrow(x)/yfact))
		csteps <- as.integer(floor(ncol(x)/xfact))
	}
	
	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
		
	outRaster <- raster(x, filename)
	bndbox <- extent(xmin(x), xmx, ymn, ymax(x))
	extent(outRaster) <- bndbox
	rowcol(outRaster) <- c(rsteps, csteps) 
	
	
	if (na.rm) {
		# this avoid warning messages 
		thefun <- function(x) { 
			x <- na.omit(x)
			if (length(x) == 0) { 
				return(NA)
			} else { 
				return( fun(x) )
			}
		}
	} else {
		thefun <- fun
	}
	
	if (dataContent(x) == 'all') {	
		cols <- rep(rep(1:csteps, each=xfact)[1:ncol(x)], times=nrow(x))
		rows <- rep(1:rsteps, each=ncol(x) * yfact)[1:ncell(x)]
		cells <- cellFromRowCol(x, rows, cols)
		
		outRaster <- setValues(outRaster, as.vector( tapply(values(x), cells, thefun )))
		
		if (filename != "") {
			outRaster <- writeRaster(outRaster, filename=filename, ...)
		}

	} else if ( dataSource(x) == 'disk') { 
		if (!canProcessInMemory(outRaster, 2) && filename == '') {
			filename <- rasterTmpFile()
									
		}
		
		cols <- rep(rep(1:csteps,each=xfact)[1:ncol(x)], times=yfact)
		rows <- rep(1, each=(ncol(x) * yfact))
		
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(outRaster), nrow=ncol(outRaster))
		} else {
			outRaster <- writeStart(outRaster, filename=filename, ...)
		}
		
		cells <- cellFromRowCol(x, rows, cols)
		nrows = yfact

		
		pb <- pbCreate(rsteps, type=.progress(...))
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
			vals <- as.vector( tapply(vals, cells, thefun ) )
			
			if (filename == "") {
				v[,r] <- vals
			} else {
				writeValues(outRaster, vals, r)
			}
		
			pbStep(pb, r) 
		} 
		pbClose(pb)
		if (filename == "") { 
			outRaster <- setValues( outRaster, as.vector(v) )
		} else {
			outRaster <- writeStop(outRaster)
		}
	}
	return(outRaster)
}
)
