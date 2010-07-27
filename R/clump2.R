# Authors: Robert J. Hijmans and Jacob van Etten, 
# Date : May 2009
# Version 0.9
# Licence GPL v3



.clump <- function(x, directions) {
	x1 <- raster(x)
	val <- which(getValues(x) != 0)
	if (length(val) == 0) { 
		return( setValues(x1, NA) )
	}
	adjv <- as.vector( t ( adjacency(x1, val, val, directions=directions) ) )
	cl <- clusters(graph(adjv, directed=FALSE))$membership[val+1]
	ucl <- sort(unique(cl))
	ucl <- data.frame(id=ucl, value=1:length(ucl))
	#test <- subset(ucl, ucl$id != ucl$value)
	#if (nrow(test) > 0 ) {
	#	cl <- merge(cl, ucl, by=1, all.x=TRUE)
	#}
	x1[val] <- cl
	return(x1)
}


.clump2 <- function(x, filename='', directions=8, ...) {

	if( !require(igraph)) {
		stop('you need to install the igraph package to be able to use this function')
	}

	if (! directions %in% c(4,8)) {stop('directions should be 4 or 8')}

	if (filename != ""  & file.exists(filename)) {
		if (.overwrite(...)==FALSE) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}

	outRaster <- raster(x)
	global <- .isGlobalLatLon(outRaster)
	
	if (canProcessInMemory(outRaster, 3)) {
		x <- .clump(x, directions)
		return(x)
	} 
	# else 
	
	outRaster <- writeStart(outRaster, filename=rasterTmpFile(), ...)

	tr <- blockSize(outRaster)
	pb <- pbCreate(tr$n, type=.progress(...))
	
	ext <- c(xmin(outRaster), xmax(outRaster), ymax(outRaster), NA)
	maxval <- 0
	
	rcl <- NULL
	for (i in 1:tr$n) {
		ext[4] <- ext[3]
		ext[3] <- yFromRow(tr$row[i] + 1) # one more row
		xc <- crop(x, ext)
		xc <- .clump(xc, directions) + maxval
		if (i > 1) {
			firstrow <- getValues(xc, 1)
			rc <- na.omit(unique(cbind(firstrow, lastrow)))
			rcl <- rbind(rcl, rc)
		}
		lastrow <- getValues(xc, nrow(xc))
		
		maxval <- max(getValues(xc))
		outRaster <- writeValues(outRaster, getValues(xc, 1, tr$nrows[i]))
	}
	outRaster <- writeStop(outRaster)

	if (filename == '') filename <- rasterTmpFile()

	# now reclass
	rcl <- graph.edgelist(rcl, directed=FALSE)
	cl <- clusters(rcl)$membership + 1
	outRaster <- reclass(outRaster, cbind(V(rcl),cl))
	return(outRaster)
}


