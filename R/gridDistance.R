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

gridDistance <- function(object, originValue, omitValue, filename="", ...) 
{
	if( !require(igraph)) {
		stop('you need to install the igraph package to be able to use this function')
	}
	
	if (missing(originValue)) stop("you must supply an 'originValue' argument")
	if (missing(omitValue)) stop("you must supply an 'omitValue' argument")
	
	
	if ((dataContent(object) != 'all') & (dataSource(object) != 'disk')) {
			stop('cannot compute distance on a RasterLayer with no data')
	}
	lonlat <- .couldBeLonLat(object)
	filename <- trim(filename)
	
	if (filename != ""  & file.exists(filename)) {
		if (! .overwrite(...)) {
			stop("file exists. Use another name or 'overwrite=TRUE' if you want to overwrite it")
		}
	}
	
	if (dataContent(object) == 'all') nr=4 else nr=5
	
	if(canProcessInMemory(object, n=nr)) {
		outRaster <- raster(object)
		object <- getValues(object) # to avoid keeping values in memory twice
		
		oC <- which(object %in% originValue) #select cells not surrounded by other origin cells
		ftC <- which(!(object %in% omitValue))
		chunkSize <- ncell(outRaster)
		outRaster <- setValues(outRaster, .calcDist(outRaster, chunkSize, ftC, oC))
		outRaster[is.infinite(outRaster)] <- NA
		if (filename != "") {
			outRaster <- writeRaster(outRaster, filename, ...)
		}
		return(outRaster)
		
	} else 	{
		tr <- blockSize(object, n=5)
		pb <- pbCreate(tr$n*2 - 1, type=.progress(...))

		#going up
		r1 <- writeStart(raster(object), rasterTmpFile(), overwrite=TRUE)
		for(i in tr$n:1) {
			chunk <- getValues(object, row=tr$row[i], nrows=tr$nrows[i]) 
			startCell <- (tr$row[i]-1) * ncol(object)
			chunkSize <- length(chunk)
			oC <- which(chunk %in% originValue) 
			ftC <- which(!(chunk %in% omitValue))
			if(i < tr$n) {
				firstRowftC <- which(!(firstRow %in% omitValue)) + chunkSize
				chunkDist <- .calcDist(object, 
								chunkSize=chunkSize+ncol(object), 
								ftC=c(ftC, firstRowftC), 
								oC=c(oC, firstRowftC), 
								perCell=c(rep(0,times=length(oC)),firstRowDist), 
								startCell=startCell)[1:chunkSize]
			} else {
				chunkDist <- .calcDist(object, 
								chunkSize=chunkSize, 
								ftC=ftC, 
								oC=oC, 
								perCell=0, 
								startCell=startCell)
			}
			firstRow <- chunk[1:nrow(object)]
			firstRowDist <- chunkDist[1:nrow(object)]
			chunkDist[is.infinite(chunkDist)] <- NA
			r1 <- writeValues(r1, chunkDist)
			pbStep(pb) 
		}
		r1 <- writeStop(r1)
		
		#going down
		if (filename == '') {filename <- rasterTmpFile()}
		
		outRaster <- writeStart(raster(object), filename=filename, overwrite=TRUE, ...)			
		for(i in 1:tr$n) {
			iM <- tr$n - i + 1
			chunk <- getValues(object, row=tr$row[i], nrows=tr$nrows[i]) 
			chunkSize <- length(chunk)
			startCell <- (tr$row[i]-1) * ncol(object)
			oC <- which(chunk %in% originValue) 
			ftC <- which(!(chunk %in% omitValue))
			chunkDist <- getValues(r1, row=tr$row[iM], nrows=tr$nrows[iM])
			chunkDist[is.na(chunkDist)] <- Inf
			if (i > 1) {
				chunkDist <- pmin(chunkDist,
					.calcDist(object, 
							chunkSize=chunkSize+ncol(object), 
							ftC=c(lastRowftC, ftC+ncol(object)), 
							oC = c(lastRowftC, oC+ncol(object)), 
							perCell=c(lastRowDist, rep(0,times=length(oC))), 
							startCell = startCell - ncol(object))[-(1:length(lastRowftC))])
				}
			lastRow <- chunk[(length(chunk)-ncol(object)+1):length(chunk)]
			lastRowDist <- chunkDist[(length(chunkDist)-ncol(object)+1):length(chunkDist)]
			lastRowftC <- which(!(lastRow %in% omitValue))
			chunkDist[is.infinite(chunkDist)] <- NA				
			outRaster <- writeValues(outRaster, chunkDist, tr$row[i])
			pbStep(pb) 
		}
		outRaster <- writeStop(outRaster)
		pbClose(pb)
		return(outRaster)
	}
}


.calcDist <- function(object, chunkSize, ftC, oC, perCell=0, startCell=0)
{
	shortestPaths <- rep(Inf, times=max(ftC))
	if(length(oC)>0)
	{
		lonlat <- .couldBeLonLat(object)
		adj <- adjacency(object,fromCells=ftC,toCells=ftC,directions=8) #OPTIMIZE: omit oC cells surrounded by other origin cells
		distGraph <- graph.edgelist(adj-1, directed=FALSE)
		if (lonlat) 
		{
			coord <- cbind(xyFromCell(object,adj[,1]+startCell),xyFromCell(object,adj[,2]+startCell))
			distance <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
			E(distGraph)$weight <- distance
		}
		else
		{
			sameRow <- rowFromCell(object, adj[,1]) == rowFromCell(object, adj[,2]) 
			sameCol <- colFromCell(object, adj[,1]) == colFromCell(object, adj[,2])
			E(distGraph)$weight[sameRow] <- xres(object)
			E(distGraph)$weight[sameCol] <- yres(object)
			E(distGraph)$weight[!sameCol & !sameRow] <- sqrt(xres(object)^2 + yres(object)^2)
		}
		shortestPaths <- pmin(shortestPaths, apply(shortest.paths(distGraph, oC-1) + perCell, 2, min))
		if(max(ftC) < chunkSize){shortestPaths <- c(shortestPaths,rep(Inf,times=chunkSize-max(ftC)))}
	}
	return(shortestPaths)
}
