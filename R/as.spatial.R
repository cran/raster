

setAs('data.frame', 'SpatialPolygons',
	function(from) {
		obs <- unique(from$object)
		sp <- list()
		for (i in 1:length(obs)) {
			s <- subset(from, object==obs[i])
			p <- unique(s$part)
			pp <- list()
			for (j in 1:length(p)) {
				ss <- subset(s, part==p[j])
				pol <- Polygon(as.matrix(ss)[,5:6])
				if (ss$hole[1]) {
					pol@hole <- TRUE
				}
				pp[[j]] <- pol
			}
			sp[[i]] <- Polygons(pp, as.character(i))
		}
		
		SpatialPolygons(sp)
	}
)


setAs('data.frame', 'SpatialPolygonsDataFrame',
	function(from) {
		x <- as(from, 'SpatialPolygons')
		if (ncol(from) > 6) {
			d <- unique(pts[, -c(3:6), drop=FALSE])
			rownames(d) <- d$object
			SpatialPolygonsDataFrame(x, d)
		} else {
			x
		}
	}
)
