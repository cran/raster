# Author: Jacob van Etten and Robert J. Hijmans
# email jacobvanetten@yahoo.com
# Date :  November 2009
# Version 0.9
# Licence GPL v3

#for data on disk only proof of concept version. In reality, we need Dijkstra´s algorithm (igraph) and read/write a couple of row at once to gain some speed

#setGeneric("gridDistance", function(x, ...) standardGeneric("gridDistance"))

#setMethod("gridDistance", signature(x = "RasterLayer"), def =	

gridDistance <- function(x, filename="", ...) {

	if ((dataContent(x) != 'all') & (dataSource(x) != 'disk')) {
		stop('cannot compute distance on a RasterLayer with no data')
	}

	filename <- trim(filename)
	
	object <- raster(x)
	n <- ncell(object)
		
	if(canProcessInMemory(object, n=5)) {
		
		x = getValues(x)
		
		outRaster <- raster(object)

		fromCells <- which(!is.na(x))
		fromCells <- fromCells[which(x[fromCells] == TRUE)]
		toCells <- which(is.na(x))
		accDist <- rep(0,times=n)
		accDist[toCells] <- Inf
		if (isLonLat(object)) {
			while(length(fromCells)>0) {			
				adj <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=8)
				coord <- cbind(xyFromCell(object,adj[,1]),xyFromCell(object,adj[,2]))
				distance <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
				#What follows is the same as for  projected  data ( further below)
				transitionValues <- accDist[adj[,1]] + distance
				transitionValues <- tapply(transitionValues,adj[,2],min)
				transitionValues <- transitionValues[transitionValues < Inf]
				index <- as.integer(names(transitionValues))
				fromCells <- index[transitionValues < accDist[index]]
				accDist[index] <- pmin(transitionValues,accDist[index])
			}
		} else {
			while(length(fromCells)>0) {			
				adj1 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=4)
				adj2 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions="Bishop")
				distance <- c(rep(1,length=length(adj1[,1])),rep(sqrt(2),length=length(adj2[,1])))
				adj <- rbind(adj1,adj2)
				#What follows is the same as for LatLon
				transitionValues <- accDist[adj[,1]] + distance
				transitionValues <- tapply(transitionValues,adj[,2],min)
				transitionValues <- transitionValues[transitionValues < Inf]
				index <- as.integer(names(transitionValues))
				fromCells <- index[transitionValues < accDist[index]]
				accDist[index] <- pmin(transitionValues,accDist[index])
			}
		}
			
		outRaster <- setValues(outRaster, accDist)	
		if (filename != "") {
			outRaster <- writeRaster(outRaster, filename=filename, ...)
		}
		return(outRaster)

	} else { 
		stop('not yet implemented for large rasters')
		maxDist <- pointDistance(xyFromCell(object,1),xyFromCell(object,ncell(object)), type='GreatCircle')
		nrows <- nrow(object)
		ncols <- ncol(object)
		func <- function(x) 
		{
			x[is.na(x)] <- maxDist
			x[x==0] <- NA
			x[x==1] <- 0
			return(x)
		}
			f1 <- rasterTmpFile()
			f2 <- rasterTmpFile()
			r1 <- calc(object, func, filename=f1, overwrite=TRUE, datatype="FLT4S")
			r2 <- raster(r1)
			remainingCells <- TRUE
			while (remainingCells) 
			{
				remainingCells <- FALSE
				r1 <- getValues(r1, 1)
				rowWindow <- values(r1)
				for(r in 1:(nrows-1))
				{
					r1 <- getValues(r1, r+1)
					rowWindow <- c(rowWindow, values(r1))
					fromCells <- ((((r-1)*ncols)+1):((r+1)*ncols))[!is.na(rowWindow) & !((maxDist - rowWindow) < 1e-60)] 
					toCells <- ((((r-1)*ncols)+1):((r+1)*ncols))[!is.na(rowWindow)] 
					if(isLonLat(object))
					{						
						adj <- adjacency(object, fromCells=fromCells, toCells=toCells, directions=8)
						coord <- cbind(xyFromCell(object,adj[,1]), xyFromCell(object, adj[,2]))
						distance <- apply(coord,1,function(x){pointDistance(x[1:2],x[3:4], type='GreatCircle')})
					} else
					{
						adj1 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions=4)
						adj2 <- adjacency(object,fromCells=fromCells,toCells=toCells,directions="Bishop")
						distance <- c(rep(1,length=length(adj1[,1])),rep(sqrt(2),length=length(adj2[,1])))
						adj <- rbind(adj1,adj2)
					}
					if(length(adj[,1]) > 0)
					{
						adj <- adj-(r-1)*ncols
						transitionValues <- as.vector(rowWindow)[adj[,1]] + distance
						transitionValues <- tapply(transitionValues,adj[,2],min)
						transitionVal <- rowWindow
						index <- as.integer(names(transitionValues))
						transitionVal[index] <- transitionValues
						newValues <- as.vector(pmin(transitionVal,rowWindow))
 						if (sum(newValues) < sum(rowWindow) - 1e-3) #in reality, this should be 0 with floating point precision. Given the terrible inefficiency of the current algorithm, we choose a high value.
						{
							remainingCells <- TRUE
						}
					} else {newValues <- rowWindow}
					r2 <- setValues(r2, newValues[1:ncols], r)
					r2 <- writeRaster(r2, f2, overwrite=TRUE)
					rowWindow <- newValues[-(1:ncols)]
				}
				r2 <- setValues(r2, rowWindow, r+1)
				r2 <- writeRaster(r2, f2, overwrite=TRUE)
				#plot(r2) #for some reason this doesn´t work, why?
				ftmp <- f2
				f2 <- f1
				f1 <- ftmp
				r1 <- raster(f1)
				r2 <- raster(r1)
			}
			outRaster <- saveAs(r1, filename=filename, ...)
			removeRasterFile(f1)	 
            removeRasterFile(f2)
			return(outRaster)
		}
	}
#)

