# Author: Robert J. Hijmans and Paul Hiemstra
# r.hijmans@gmail.com
# Date :  January 2009
# Version 0.9
# Licence GPL v3


pointsToRaster <- function(raster, xy, values=1, fun=length, background=NA, filename="", ...) {

	rs <- raster(raster)
	
	nres <- length(fun(1))
	xy <- .pointsToMatrix(xy)
	
	if (is.atomic(values) & length(values)==1) {
		values <- rep(values, dim(xy)[1])
	}
	if (dim(xy)[1] != length(values)) {
		stop('number of points does not match the number of values')
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
		xyarc <- cbind(xy, values, rows, cols)
		urows <- unique(rows)
#		urows <- urows[order(urows)]
		if (nres==1) {
			dna <- vector(length=ncol(rs))
			dna[] <- background
		} else {
			rs <- brick(rs)  #  return a'RasterBrick'
			dna <- matrix(background, nrow=ncol(rs), ncol=nres)
		}
		pb <- pbCreate(nrow(rs), type=.progress(...))
		rs <- writeStart(rs, filename=filename, ...)
		for (r in 1:rs@nrows) {
			d <- dna
			if (r %in% urows) {
				ss <- subset(xyarc, xyarc[,4] == r)
				#ucols <- unique(ss[,5])
				#for (c in 1:length(ucols)) {
				#	sss <- subset(ss, ss[,5] == ucols[c] )
				#	d[ucols[c]] <- fun(sss[,3])	
				#}
				
				v = tapply(ss[,3], ss[,5], fun)
				cells <- as.numeric(rownames(v))
				
				if (nres > 1) {
					v <- as.matrix(v)
					v = t(apply(v, 1, function(x) x[[1]]))  # Reshape the data if more than one value is returned by 'fun'
					d[cells, ] <- v
				} else {
					d[cells] <- v
				}
			}
			rs <- writeValues(rs, d) 
			pbStep(pb, r)
		}
		rs <- writeStop(rs)
		pbClose(pb)
		
	} else {
		v = tapply(values, cells, fun)
		cells <- as.numeric(rownames(v))
		v <- as.matrix(v)
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
		
		if (filename != "") {
			rs <- writeRaster(rs, filename=filename, ...)
		}
	}
	return(rs)	
}

