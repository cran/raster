# Author: Jacob van Etten
# email jacobvanetten@yahoo.com
# Date :  May 2010
# Version 1.0.x
# Licence GPL v3

#the igraph method is speedy for a low number of origin cells, as it calculates shortest distances for each origin cell individually. 
#We could ask the igraph author to make it possible to calculate shortest distance for several origin nodes simultaneously. 
#However, I think I asked this more than a year ago and he seemed not very convinced.

# We could -- perhaps -- speed it up for cases with many origin cells by only considering the 'edge' cells (all the others have distance = 0
# see the edge function. Or perhaps use igraph to compute the edges??? 

#setGeneric("gridDistance", function(object, ...) standardGeneric("gridDistance"))

#setMethod("gridDistance", signature(object = "RasterLayer"), def =	

gridDistance <- function(x, origin, omit=NULL, filename="", ...) {

	if( !require(igraph)) {
		stop('you need to install the igraph package to be able to use this function')
	}
	
	if (missing(origin)) stop("you must supply an 'origin' argument")
	
	if ((! inMemory(x) ) & ( !  fromDisk(x) )) {
			stop('cannot compute distance on a RasterLayer with no data')
	}
	lonlat <- .couldBeLonLat(x)
	filename <- trim(filename)
	
	if (filename != ""  & file.exists(filename)) {
		if (! .overwrite(...)) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}
	
	if ( nrow(x) <= 100 ) { #canProcessInMemory(x, n=25) ) {
		outRaster <- raster(x)
		x <- getValues(x) # to avoid keeping values in memory twice
		
		oC <- which(x %in% origin) #select cells not surrounded by other origin cells
		ftC <- which(!(x %in% omit))
		v <- .calcDist(outRaster, ncell(outRaster), ftC, oC, lonlat=lonlat)
		
		outRaster <- setValues(outRaster, v)
		outRaster[is.infinite(outRaster)] <- NA
		if (filename != "") {
			outRaster <- writeRaster(outRaster, filename, ...)
		}
		return(outRaster)
		
	} else 	{
	
		tr <- blockSize(x, n=10, minblocks=nrow(x)/10)
		
		pb <- pbCreate(tr$n*2 - 1, type=.progress(...))

		#going up
		r1 <- writeStart(raster(x), rasterTmpFile(), overwrite=TRUE)
		for (i in tr$n:1) {
			chunk <- getValues(x, row=tr$row[i], nrows=tr$nrows[i]) 
			startCell <- (tr$row[i]-1) * ncol(x)
			chunkSize <- length(chunk)
			oC <- which(chunk %in% origin) 
			ftC <- which(!(chunk %in% omit))
			if (i < tr$n) {
				firstRowftC <- which(!(firstRow %in% omit)) + chunkSize
				chunkDist <- .calcDist(x, 
								chunkSize=chunkSize + ncol(x), 
								ftC=c(ftC, firstRowftC), 
								oC=c(oC, firstRowftC), 
								perCell=c(rep(0,times=length(oC)),firstRowDist), 
								startCell=startCell,
								lonlat=lonlat)[1:chunkSize]
			} else {
				chunkDist <- .calcDist(x, 
								chunkSize=chunkSize, 
								ftC=ftC, 
								oC=oC, 
								perCell=0, 
								startCell=startCell,
								lonlat=lonlat)
			}
			firstRow <- chunk[1:nrow(x)]
			firstRowDist <- chunkDist[1:nrow(x)]
			chunkDist[is.infinite(chunkDist)] <- NA
			r1 <- writeValues(r1, chunkDist)
			pbStep(pb) 
		}
		r1 <- writeStop(r1)
		
		#going down
		if (filename == '') {filename <- rasterTmpFile()}
		
		outRaster <- writeStart(raster(x), filename=filename, overwrite=TRUE, ...)			
		for (i in 1:tr$n) {
			iM <- tr$n - i + 1
			chunk <- getValues(x, row=tr$row[i], nrows=tr$nrows[i]) 
			chunkSize <- length(chunk)
			startCell <- (tr$row[i]-1) * ncol(x)
			oC <- which(chunk %in% origin) 
			ftC <- which(!(chunk %in% omit))
			chunkDist <- getValues(r1, row=tr$row[iM], nrows=tr$nrows[iM])
			chunkDist[is.na(chunkDist)] <- Inf
			if (i > 1) {
				chunkDist <- pmin(chunkDist,
					.calcDist(x, 
							chunkSize=chunkSize+ncol(x), 
							ftC=c(lastRowftC, ftC+ncol(x)), 
							oC = c(lastRowftC, oC+ncol(x)), 
							perCell=c(lastRowDist, rep(0,times=length(oC))), 
							startCell = startCell - ncol(x),
							lonlat=lonlat)[-(1:length(lastRowftC))])
				}
			lastRow <- chunk[(length(chunk)-ncol(x)+1):length(chunk)]
			lastRowDist <- chunkDist[(length(chunkDist)-ncol(x)+1):length(chunkDist)]
			lastRowftC <- which(!(lastRow %in% omit))
			chunkDist[is.infinite(chunkDist)] <- NA				
			outRaster <- writeValues(outRaster, chunkDist, tr$row[i])
			pbStep(pb) 
		}
		outRaster <- writeStop(outRaster)
		pbClose(pb)
		return(outRaster)
	}
}


.calcDist <- function(x, chunkSize, ftC, oC, perCell=0, startCell=0, lonlat) {
	shortestPaths <- rep(Inf, times=max(ftC))
	
	if(length(oC)>0) {
	
		adj <- adjacency(x, fromCells=ftC, toCells=ftC, directions=8) #OPTIMIZE: omit oC cells surrounded by other origin cells
		distGraph <- graph.edgelist(adj-1, directed=FALSE)
		
		if (lonlat) {
			# coord <- cbind(xyFromCell(x,adj[,1]+startCell),xyFromCell(x,adj[,2]+startCell))
			# distance1 <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
			# orders of magnitude faster than apply
			distance <- pointDistance(xyFromCell(x,adj[,1]+startCell), xyFromCell(x,adj[,2]+startCell), type='GreatCircle') 
			E(distGraph)$weight <- distance
		
		} else {
			sameRow <- rowFromCell(x, adj[,1]) == rowFromCell(x, adj[,2]) 
			sameCol <- colFromCell(x, adj[,1]) == colFromCell(x, adj[,2])
			E(distGraph)$weight[sameRow] <- xres(x)
			E(distGraph)$weight[sameCol] <- yres(x)
			E(distGraph)$weight[!sameCol & !sameRow] <- sqrt(xres(x)^2 + yres(x)^2)
		}
		
		shortestPaths <- pmin(shortestPaths, apply(shortest.paths(distGraph, oC-1) + perCell, 2, min))
		
		if(max(ftC) < chunkSize){ 
			shortestPaths <- c( shortestPaths, rep(Inf, times=chunkSize-max(ftC)) )
		}
	}
	return(shortestPaths)
}
