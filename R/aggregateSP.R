
setMethod('aggregate', signature(x='SpatialPolygons'), 
function(x, v=colnames(x@data), ...) {
	require(rgeos)
	
	if (! .hasSlot(x, 'data') ) {
		if (version_GEOS0() < "3.3.0") {
			x <- gUnionCascaded(x)
		} else {
			x <- gUnaryUnion(x)
		}	
		return(x)
	
	} else {
		if (isTRUE(is.null(v)) | isTRUE(is.na(v))) {
			if (version_GEOS0() < "3.3.0") {
				x <- gUnionCascaded(x)
			} else {
				x <- gUnaryUnion(x)
			}	
			x <- SpatialPolygonsDataFrame(x, data=data.frame(id=1))
			return(x)
			
		} else {
			dat <- x@data
			vl <- length(v)
			v <- unique(v)
			if (is.numeric(v)) {
				v <- round(v)
				v <- v[v>0 & v <= ncol(dat)]
				if (length(v) < 1) {
					stop('v has no valid column numbers')
				}
			} else if (is.character(v)) {
				v <- v[v %in% colnames(dat)]
				if (length(v) < 1) {
					stop('v has no valid column names')
				}
			}
			if (length(v) < vl) {
				warning('not all variables were unique or valid')
			}
			
			dat <- dat[,v]
			crs <- x@proj4string
			dc <- apply(dat, 1, function(y) paste(as.character(y), collapse='_'))
			dc <- data.frame(oid=1:length(dc), v=dc)
			dc[,2] <- as.integer(dc[,2])
			ud <- data.frame(v=unique(dc[,2]))
			md <- merge(dc, ud, by='v')
			md <- md[order(md$oid), ]
			id <- md[!duplicated(md[,1]),]
			id <- id[order(id$v), ]
			dat <- dat[id[,2], ]
			
			if (version_GEOS0() < "3.3.0") {
				x <- lapply(1:nrow(ud), function(y) gUnionCascaded(x[md[md$v==y,2],]))
			} else {
				x <- lapply(1:nrow(ud), function(y) gUnaryUnion(x[md[md$v==y,2],]))
			}	
			
			x <- sapply(1:length(x), function(y) x[[y]]@polygons[[1]]@Polygons)
			x <- SpatialPolygons(lapply(1:length(x), function(y) Polygons(x[[y]], y)))
			x@proj4string <- crs
			
			rownames(dat) <- 1:nrow(dat)
			
			SpatialPolygonsDataFrame(x, data=dat)
		}
	}
}
)


