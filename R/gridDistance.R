# Author: Jacob van Etten
# email jacobvanetten@yahoo.com
# Date :  May 2010
# Version 1.0.x
# Licence GPL v3

#setGeneric("gridDistance", function(object, ...) standardGeneric("gridDistance"))

#setMethod("gridDistance", signature(object = "RasterLayer"), def =	

gridDistance <- function(x, origin, omit=NULL, filename="", ...) {

	if( !require(igraph0)) {
		stop('you need to install the igraph0 package to be able to use this function')
	}
	
	if (missing(origin)) stop("you must supply an 'origin' argument")
	
	if (! hasValues(x) ) {
		stop('cannot compute distance on a RasterLayer with no data')
	}
	lonlat <- .couldBeLonLat(x)
	filename <- trim(filename)
	
	if (filename != ""  & file.exists(filename)) {
		if (! .overwrite(...)) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}
	
	# keep canProcessInMemory for debugging
	if ( canProcessInMemory(x, n=10) ) { # need to test more to see how much igraph can deal with
		outRaster <- raster(x)
		x <- getValues(x) # to avoid keeping values in memory twice
		
		oC <- which(x %in% origin) 
		ftC <- which(!(x %in% omit))
		v <- .calcDist(outRaster, ncell(outRaster), ftC, oC, lonlat=lonlat)
		v[is.infinite(v)] <- NA
		
		outRaster <- setValues(outRaster, v)
		if (filename != "") {
			outRaster <- writeRaster(outRaster, filename, ...)
		}
		return(outRaster)
		
	} else 	{
	
		tr <- blockSize(x, n=1)
		
		pb <- pbCreate(tr$n*2 - 1, ...)

		#going up
		r1 <- writeStart(raster(x), rasterTmpFile(), overwrite=TRUE)
		for (i in tr$n:1) {
			chunk <- getValues(x, row=tr$row[i], nrows=tr$nrows[i]) 
			startCell <- (tr$row[i]-1) * ncol(x)
			chunkSize <- length(chunk)
			oC <- which(chunk %in% origin) 
			ftC <- which(!(chunk %in% omit))
			if (length(ftC) != 0) {

				if (i < tr$n) {
					firstRowftC <- firstRowftC + chunkSize 
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
			} else {
				if (i < tr$n) {
					firstRowftC <- firstRowftC + chunkSize 
				}
				chunkDist <- rep(NA, tr$nrows[i] * ncol(r1))
			}
			firstRow <- chunk[1:ncol(x)]
			firstRowDist <- chunkDist[1:ncol(x)]
			firstRowftC <- which(!(firstRow %in% omit))
			firstRowDist <- firstRowDist[firstRowftC]
			chunkDist[is.infinite(chunkDist)] <- NA

			r1 <- writeValues(r1, chunkDist, tr$row[i])
			pbStep(pb) 
		}
		r1 <- writeStop(r1)
		
		#going down
		if (filename == '') {filename <- rasterTmpFile()}
		
		outRaster <- writeStart(raster(x), filename=filename, overwrite=TRUE, ...)			
		for (i in 1:tr$n) {
			chunk <- getValues(x, row=tr$row[i], nrows=tr$nrows[i]) 
			chunkSize <- length(chunk)
			startCell <- (tr$row[i]-1) * ncol(x)
			oC <- which(chunk %in% origin) 
			ftC <- which(!(chunk %in% omit))
			
			if (length(ftC) != 0) {
			
				if (i > 1) {
					chunkDist <- getValues(r1, row=tr$row[i], nrows=tr$nrows[i]) 
					chunkDist[is.na(chunkDist)] <- Inf 
				
					chunkDist <- pmin(chunkDist,
						.calcDist(x, 
							chunkSize=chunkSize+ncol(x), 
							ftC = c(lastRowftC, ftC+ncol(x)), 
							oC = c(lastRowftC, oC+ncol(x)), 
							perCell=c(lastRowDist, rep(0,times=length(oC))), 
							startCell = startCell - ncol(x),
							lonlat=lonlat)[-(1:ncol(r1))])
							
				} else {
				
					chunkDist <- getValues(r1, row=tr$row[i], nrows=tr$nrows[i])
					chunkDist[is.na(chunkDist)] <- Inf
			
					chunkDist <- pmin(chunkDist,
						.calcDist(x, 
							chunkSize=chunkSize, 
							ftC=ftC, 
							oC=oC, 
							perCell=0, 
							startCell=startCell,
							lonlat=lonlat))
				}
			
				
			} else {
				chunkDist <- rep(NA, tr$nrows[i] * ncol(outRaster))		
				
			}

			lastRow <- chunk[(length(chunk)-ncol(x)+1):length(chunk)]
			lastRowDist <- chunkDist[(length(chunkDist)-ncol(x)+1):length(chunkDist)]
			lastRowftC <- which(!(lastRow %in% omit))
			lastRowDist <- lastRowDist[lastRowftC]
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
	
	if(length(oC)>0) {
	
		adj <- adjacency(x, fromCells=ftC, toCells=ftC, directions=8)
		startNode <- max(adj)+1 #extra node to serve as origin
		adjP <- rbind(adj, cbind(rep(startNode, times=length(oC)), oC))
		distGraph <- graph.edgelist(adjP-1, directed=TRUE)
		if(length(perCell) == 1) {
			if(perCell == 0) {perCell <- rep(0, times=length(oC))}
		}
		
		if (lonlat) {
			distance <- pointDistance(xyFromCell(x,adj[,1]+startCell), xyFromCell(x,adj[,2]+startCell), longlat=TRUE) 
			
			E(distGraph)$weight <- c(distance, perCell)
			
		} else {
			sameRow <- which(rowFromCell(x, adj[,1]) == rowFromCell(x, adj[,2]))
			sameCol <- which(colFromCell(x, adj[,1]) == colFromCell(x, adj[,2]))
			E(distGraph)$weight <- sqrt(xres(x)^2 + yres(x)^2)
			E(distGraph)$weight[sameRow] <- xres(x)
			E(distGraph)$weight[sameCol] <- yres(x)
			E(distGraph)$weight[(length(adj[,1])+1):(length(adj[,1])+length(oC))] <- perCell
		}
		
		shortestPaths <- shortest.paths(distGraph, startNode-1)
		shortestPaths <- shortestPaths[-(length(shortestPaths))] #chop startNode off
		
		if(length(shortestPaths) < chunkSize){ 
			#add Inf values where shortest.paths() leaves off before completing all nodes
			shortestPaths <- c(shortestPaths, rep(Inf, times=chunkSize-length(shortestPaths))) 
		}
		
	} else {
	
		shortestPaths <- rep(Inf, times=chunkSize)
	
	}
	
	return(shortestPaths)

}
