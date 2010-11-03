# Author: Robert J. Hijmans, Paul Hiemstra, Steven Mosher
# r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


pointsToRaster <- function(raster, xy, values=1, fun=NULL, background=NA, na.rm=TRUE, filename="", ...) {

	rs <- raster(raster)
	
	if (is.null(fun)) {
		if (na.rm) {
			fun=function(x, ...){ length(na.omit(x)) }
		} else {
			fun=function(x, ...){ length(x) }
		}
	}
	
	nres <- length(fun(1))
	xy <- .pointsToMatrix(xy)

	ncols <- 1
	if (NCOL(values) > 1) {
		if (nres > 1) stop('Either use a single function, or a vector for values')
		nres <- ncols <- ncol(values)
	} else {
		if (is.atomic(values) & length(values)==1) {
			values <- rep(values, dim(xy)[1])
		}
		if (dim(xy)[1] != length(values)) {
			stop('number of points does not match the number of values')
		}
	}
	
	
	cells <- cellFromXY(rs, xy)
	
#	todisk <- TRUE
	todisk <- FALSE
	if (!canProcessInMemory(rs, 2 * nres))  {
		if (filename == '') {
			filename <- rasterTmpFile()
		}
		todisk <- TRUE
	}	
	
	
	if (todisk) {
		rows <- rowFromCell(rs, cells)
		cols <- colFromCell(rs, cells)
		xyarc <- cbind(xy, rows, cols, values)
		urows <- unique(rows)
#		urows <- urows[order(urows)]
		if (nres==1) {
			dna <- vector(length=ncol(rs))
			dna[] <- background
		} else {
			rs <- brick(rs)  #  return a'RasterBrick'
			rs@data@nlayers <- nres
			if (ncols > 1) { rs@layernames <- colnames(values) }
			dna <- matrix(background, nrow=ncol(rs), ncol=nres)
			datacols <- 5:ncol(xyarc)
		}
		pb <- pbCreate(nrow(rs), type=.progress(...))
		rs <- writeStart(rs, filename=filename, ...)
		for (r in 1:rs@nrows) {
			d <- dna
			if (r %in% urows) {
				ss <- subset(xyarc, xyarc[,3] == r)
				#ucols <- unique(ss[,5])
				#for (c in 1:length(ucols)) {
				#	sss <- subset(ss, ss[,5] == ucols[c] )
				#	d[ucols[c]] <- fun(sss[,3])	
				#}
				
				if (ncols > 1) {
					v <- aggregate(ss[,datacols,drop=FALSE], list(ss[,4]), fun, na.rm=na.rm)
					cells <- as.numeric(v[,1])
					d[cells, ] <- as.matrix(v)[,-1]
				} else {
					v = tapply(ss[,5], ss[,4], fun, na.rm=na.rm)
					cells <- as.numeric(rownames(v))
					if (nres > 1) {
						v <- as.matrix(v)
						v = t(apply(v, 1, function(x) x[[1]]))  # Reshape the data if more than one value is returned by 'fun'
						d[cells, ] <- v
					} else {
						d[cells] <- v
					}
				}
			}
			rs <- writeValues(rs, d, r) 
			pbStep(pb, r)
		}
		rs <- writeStop(rs)
		pbClose(pb)
		
	} else {
	
		if (ncols > 1) {
			v <- aggregate(values, list(cells), fun, na.rm=na.rm)
			cells <- as.numeric(v[,1])
			v <- as.matrix(v)[,-1,drop=FALSE]
		} else {
			v <- tapply(values, cells, fun, na.rm=na.rm)
			cells <- as.numeric(rownames(v))
			v <- as.matrix(v)
		}
		
		if(class(v[1]) == "list") {
			v = t(apply(v, 1, function(x) x[[1]]))  # Reshape the data if more than one value is returned by 'fun'
		}

		if (dim(v)[2] > 1) { 
			vv <- matrix(background, nrow=ncell(rs), ncol=dim(v)[2])
			vv[cells, ] <- v
		    rs <- brick(rs)  #  return a'RasterBrick'
		} else {
			vv <- 1:ncell(rs)
			vv[] <- background
			vv[cells] <- v
		}
		rs <- setValues(rs, vv)
		if (ncols > 1) {
			cn <- colnames(values)
			if (! is.null(cn)) {
				rs@layernames = cn
			}	
		}

		if (filename != "") {
			rs <- writeRaster(rs, filename=filename, ...)
		}
	}
	return(rs)	
}

