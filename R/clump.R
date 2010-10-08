# Authors: Robert J. Hijmans and Jacob van Etten, 
# Date : May 2010
# Version 0.9
# Licence GPL v3


if (!isGeneric("clump")) {
	setGeneric("clump", function(x, ...)
		standardGeneric("clump"))
}	


.smallClump <- function(x, directions=8) {
	x1 <- raster(x)
	val <- which(getValues(x) != 0)
	if (length(val) == 0) { 
		return( setValues(x1, NA) )
	}
	adjv <- as.vector( t ( adjacency(x1, val, val, directions=directions) ) )
	cl <- clusters(graph(adjv, directed=FALSE))$membership[val+1]
	
	add = val[! val %in% adjv]		   # RH
	adjv <- c(adjv, rep(add, each=2))  # fixed problem of missing the last single cells, perhaps clumsy?
	
	cl <- clusters(graph(adjv, directed=FALSE))$membership[val+1]
	#ucl <- sort(unique(cl))
	#ucl <- data.frame(id=ucl, value=1:length(ucl))

	x1[val] <- as.numeric(as.factor(cl)) # RH force 1 to n
	return(x1)
}


setMethod('clump', signature(x='RasterLayer'), 

function(x, filename='', directions=8, gaps=TRUE, datatype='INT2U', ...) {

	if( !require(igraph)) {
		stop('you need to install the igraph package to be able to use this function')
	}

	if (! directions %in% c(4,8)) { stop('directions should be 4 or 8') }

	filename <- trim(filename)
	if (filename != ""  & file.exists(filename)) {
		if (.overwrite(...)==FALSE) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}

	outRaster <- raster(x)
	
	if (canProcessInMemory(outRaster, 3)) {
		x <- .smallClump(x, directions)
		if (filename != '') {
			x <- writeRaster(x, filename, ...)
		}
		return(x)
	} 
	# else 

	outRaster <- writeStart(outRaster, filename=rasterTmpFile(), datatype='INT2U')

	tr <- blockSize(outRaster)
	pb <- pbCreate(tr$n, type=.progress(...))
	
	ext <- c(xmin(outRaster), xmax(outRaster), ymax(outRaster), NA)
	maxval <- 0
	
	rcl <- matrix(nrow=0, ncol=2)
	
	for (i in 1:tr$n) {
	
		ext[4] <- yFromRow(outRaster, tr$row[i]) + 0.5 * yres(outRaster)
		
		endrow <- tr$row[i] + tr$nrows[i] - 1 
		ext[3] <- yFromRow(outRaster, endrow) - 1.5 * yres(outRaster) # one additional row for overlap
		xc <- crop(x, extent(ext))
		
		xc <- .smallClump(xc, directions) + maxval
		if (i > 1) {
			firstrow <- getValues(xc, 1)
			rc <- na.omit(unique(cbind(lastrow, firstrow)))
			rcl <- rbind(rcl, rc)
		}
		lastrow <- getValues(xc, nrow(xc))
		
		maxval <- maxValue(xc)
		outRaster <- writeValues(outRaster, getValues(xc, 1, tr$nrows[i]), tr$row[i])
		pbStep(pb)
	}
	outRaster <- writeStop(outRaster)
	pbClose(pb)
	
	
	if (nrow(rcl) > 0) {
		g <- graph.edgelist(rcl, directed=FALSE)
		cl <- clusters(g)$membership
		rc <- cbind(V(g),cl)
		i <- rc[,1] != rc[,2]
		rc <- rc[i, ,drop=FALSE]
		outRaster <- subs(outRaster, data.frame(rc), subsWithNA=FALSE, filename=filename, datatype=datatype, ...)
		return(outRaster)
		
	} else if (!gaps) {
		un <- unique(outRaster)
		un <- data.frame(cbind(un, 1:length(un)))
		return( subs(outRaster, un, subsWithNA=FALSE, filename=filename, datatype=datatype, ...) )
		
		
	} else if (filename != '') {
		return( writeRaster(outRaster, filename=filename, datatype=datatype, ...) )
		
	} else {
		return(outRaster)
	}
}

)


