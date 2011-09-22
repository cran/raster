# Author: Robert J. Hijmans
# Date :  September 2011
# Version 1.0
# Licence GPL v3


adjacent <- function(x, cells, directions=4, pairs=TRUE, target) {

	if (is.character(directions)) { directions <- tolower(directions) }

	x <- raster(x)
	r <- res(x)
	xy <- xyFromCell(x, cells)

	if (directions==4) {
		d <- c(xy[,1]-r[1], xy[,1]+r[1], xy[,1], xy[,1], xy[,2], xy[,2], xy[,2]+r[2], xy[,2]-r[2])
		
	} else if (directions==8) {
		d <- c(rep(xy[,1]-r[1], 3), rep(xy[,1]+r[1],3), xy[,1], xy[,1], 
			rep(c(xy[,2]+r[2], xy[,2], xy[,2]-r[2]), 2),  xy[,2]+r[2], xy[,2]-r[2])

	} else if (directions==16) {
		r2 <- r * 2
		d <- c(rep(xy[,1]-r2[1], 2), rep(xy[,1]+r2[1], 2),
			rep(xy[,1]-r[1], 5), rep(xy[,1]+r[1], 5),
			xy[,1], xy[,1], 
							
			rep(c(xy[,2]+r[2], xy[,2]-r[2]), 2),
			rep(c(xy[,2]+r2[2], xy[,2]+r[2], xy[,2], xy[,2]-r[2], xy[,2]-r2[2]), 2),
			xy[,2]+r[2], xy[,2]-r[2])
							
							
	} else if (directions=='bishop') {
		d <- c(rep(xy[,1]-r[1], 2), rep(xy[,1]+r[1],2), rep(c(xy[,2]+r[2], xy[,2]-r[2]), 2))
		directions <- 4 # to make pairs
		
	} else {
		stop('directions should be one of: 4, 8, 16, or bishop')
	}
	
	d <- matrix(d, ncol=2)
	
	if (.isGlobalLonLat(x)) {
		# normalize longitude to -180..180
		d[,1] <- (d[,1] + 180) %% 360 - 180
	}
	
	if (pairs) {
		cells <- rep(cells, directions)
		d <- na.omit(cbind(cells, cellFromXY(x, d)))
		colnames(d) <- c('from', 'to')
		if (!missing(target)) {
			d <- d[d[,2] %in% target, ]
		}
		d <- d[order(d[,1], d[,2]),]
	} else {
		d <- as.vector(unique(na.omit(cellFromXY(x, d))))
		if (!missing(target)) {
			d <- d[d %in% target]
		}
	}
	d
}

