# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : July 2010
# Version 1.0
# Licence GPL v3

setMethod('aggregate', signature(x='Raster'), 
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
		lastcol <- x@ncols
		lastrow <- x@nrows
	} else 	{
		rsteps <- as.integer(floor(nrow(x)/yfact))
		csteps <- as.integer(floor(ncol(x)/xfact))
		lastcol <- min(csteps * xfact, x@ncols)
		lastrow <- min(rsteps * yfact, x@nrows)
	}
	
	ymn <- ymax(x) - rsteps * yfact * yres(x)
	xmx <- xmin(x) + csteps * xfact * xres(x)
		
	nl <- nlayers(x)
	if (nl > 1) {
		out <- brick(x, values=FALSE)
	} else {
		out <- raster(x)		
	}
	extent(out) <- extent(xmin(x), xmx, ymn, ymax(x))
	dim(out) <- c(rsteps, csteps) 
	layerNames(out) <- layerNames(x)

	if (! hasValues(x) ) {	return(out) }	
	
	if (nl < 2) {	

		fun <- .makeTextFun(fun)
		if (class(fun) == 'character') { 
			rowcalc <- TRUE 
			fun <- .getRowFun(fun)
		} else { 
			rowcalc <- FALSE 
		}

		if (! canProcessInMemory(x)) {
			if (filename == '') { 
				filename <- rasterTmpFile() 
			}
		}
			
		if (filename == '') {
			v <- matrix(NA, ncol=nrow(out), nrow=ncol(out))
		} else {
			out <- writeStart(out, filename=filename, ...)
		}
			
		pb <- pbCreate(rsteps, ...)
			
			#vv <- matrix(ncol= csteps * yfact, nrow=rsteps * xfact)
		vv <- matrix(nrow= yfact * xfact, ncol=csteps)

		w <- getOption('warn')
		on.exit(options('warn' = w))
		options('warn'=-1) 

		for (r in 1:rsteps) {
			
			startrow <- 1 + (r - 1) * yfact
			vals <- getValuesBlock(x, startrow, yfact, 1, lastcol)
				
			if (r==rsteps) { 
				endrow <- min(x@nrows, (startrow + yfact - 1))
				nrows <- endrow - startrow + 1
				vals <- matrix(vals, nrow=nrows, byrow=TRUE )
				vv[] <- NA 
				vvv <- vv[1:(nrows*xfact), ,drop=FALSE]
				vvv[1:length(vals)] <- vals
				vv[1:nrow(vvv),] <- vvv
							
			} else {
				vals <- matrix(vals, nrow=yfact, byrow=TRUE )
				vv[1:length(vals)] = vals
			}
				
			if (rowcalc) {
				vals <- fun(t(vv), na.rm=na.rm )
			} else {
				vals <- apply(vv, 2, fun, na.rm=na.rm )
			}
			if (filename == "") {
				v[, r] <- vals
			} else {
				out <- writeValues(out, vals, r)
			}
			pbStep(pb, r) 
		} 
			
		pbClose(pb)
		if (filename == "") { 
			values(out) <- as.vector(v)
		} else {
			out <- writeStop(out)
		}
		return(out)
	
	} else { # nlayers > 1
	
		
		if (canProcessInMemory(x, nlayers(x)+2)) {
		
			xx <- raster(x)		
			x <- getValues(x)
			cols <- rep(rep(1:csteps, each=xfact)[1:ncol(xx)], times=nrow(xx))
			rows <- rep(1:rsteps, each=ncol(xx) * yfact)[1:ncell(xx)]
			cells <- cellFromRowCol(xx, rows, cols)
			
			x <- as.matrix( aggregate(x, list(cells), fun, na.rm=na.rm ))[,-1]
			rm(cells)
			
			x <- setValues(out, x)
			if (filename != "") {
				x <- writeRaster(x, filename=filename, ...)
			}
			return(x)

		} else  { 
		
			cols <- rep(rep(1:csteps,each=xfact)[1:ncol(x)], times=yfact)
			rows <- rep(1, each=(ncol(x) * yfact))
			
			out <- writeStart(out, filename=filename, ...)
			
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
			
				out <- writeValues(out, vals, r)
				pbStep(pb, r) 
			} 
			pbClose(pb)
			out <- writeStop(out)
			return(out)
		}
	
	}
}
)
