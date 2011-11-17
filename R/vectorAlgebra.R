
setMethod("+", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		require(rgeos)
		merge(e1, e2)
	}
)


setMethod("*", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		require(rgeos)
		crop(e1, e2)
	}
)


setMethod("-", signature(e1='SpatialPolygons', e2='SpatialPolygons'),
    function(e1, e2){ 
		require(rgeos)
		if (!.hasSlot(e1, 'data')) {
			d <- data.frame(ID=1:length(e1@polygons))
			rownames(d) <- row.names(e1)
			e1 <- SpatialPolygonsDataFrame(e1, data=d)
			dropframe <- TRUE
		} else {
			dropframe <- FALSE
		}

		int <- gIntersects(e1, e2, byid=TRUE)
		int1 <- apply(int, 1, any)
		int2 <- apply(int, 2, any)
		
		
		part1 <- e1[!int,]
		part2 <- gDifference(e1[int,], e2, byid=TRUE)
		ids <- sapply(row.names(part2), function(x) strsplit(x, ' ')[[1]][1])
		part2 <- spChFIDs(part2, ids)
		part2 <- SpatialPolygonsDataFrame(part2, e1@data[match(ids, rownames(e1@data)),])
		part1 <- rbind(part1, part2)
		part1 <- aggregate(part1, v=colnames(part1@data))
		if (dropframe) {
			as(part1, 'SpatialPolygons')
		} else {
			part1
		}
	}
)


