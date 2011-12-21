# raster package
# Date : October 2008
# Version 0.9
# Licence GPL v3



setMethod('disaggregate', signature(x='SpatialPolygons'), 
function(x, ...) {
	n <- length(x@polygons)
	nn <- vector(length=n)
	pp <- list()
	names <- row.names(x)
	hasdf <- .hasSlot(x, 'data')
	df <- NULL
	
	for (i in 1:n) {
		p <- x@polygons[i][[1]]
		nn[i] <- length(p@Polygons)
		if (nn[i] > 1) {
			nms <- paste(names[i], 1:nn[i], sep='_')
			p <- lapply(1:length(p@Polygons), function(i) Polygons(list(p@Polygons[[i]]), nms[i]))
		} 
		pp <- c(pp, p)
		if (hasdf) {
			df <- rbind(df, x@data[rep(i,nn[i]),])
		}

	}
	pp <- SpatialPolygons(pp)
	pp@proj4string <- x@proj4string
	if (hasdf) {
		rownames(df) <- row.names(pp)
		pp <- SpatialPolygonsDataFrame(pp, df)
	}
	return(pp)
}
)


setMethod('disaggregate', signature(x='SpatialLines'), 
function(x, ...) {
	n <- length(x@lines)
	nn <- vector(length=n)
	pp <- list()
	names <- row.names(x)
	hasdf <- .hasSlot(x, 'data')
	df <- NULL
	
	for (i in 1:n) {
		p <- x@lines[i][[1]]
		nn[i] <- length(p@Lines)
		if (nn[i] > 1) {
			nms <- paste(names[i], 1:nn[i], sep='_')
			p <- lapply(1:length(p@Lines), function(i) Lines(list(p@Lines[[i]]), nms[i]))
		} 
		pp <- c(pp, p)
		if (hasdf) {
			df <- rbind(df, x@data[rep(i,nn[i]),])
		}

	}
	pp <- SpatialLines(pp)
	pp@proj4string <- x@proj4string
	if (hasdf) {
		rownames(df) <- row.names(pp)
		pp <- SpatialLinesDataFrame(pp, df)
	}
	return(pp)
}
)
