
.destPoint <- function (p, d, b=90, r=6378137) {
    toRad <- pi/180
    lon1 <- p[, 1] * toRad
    lat1 <- p[, 2] * toRad
    b <- b * toRad
    lat2 <- asin(sin(lat1) * cos(d/r) + cos(lat1) * sin(d/r) * cos(b))
    lon2 <- lon1 + atan2(sin(b) * sin(d/r) * cos(lat1), cos(d/r) - sin(lat1) * sin(lat2))
    lon2 <- (lon2 + pi)%%(2 * pi) - pi
    cbind(lon2, lat2)/toRad
}


.scalebar <- function(object, xy=click(), length=100000, label='100 km', offset=0.3, lwd=4, ... ) {
	object <- raster(object)
	if (.couldBeLonLat(object)) {
		midy <- object@extent@ymax - 0.5 * (object@extent@ymax - object@extent@ymin)
		p <- cbind(0, midy)
		d <- .destPoint(p, length)
		length <- d[1,1]
	}
	xy2 <- xy
	xy2[1,1] <- xy2[1,1] + length
	lines(rbind(xy, xy2), lwd=lwd, ...)
	xy[1,1] <- xy[1,1] + 0.5 * length
	xy[1,2] <- xy[1,2] + offset * length
	text(xy[1,1], xy[1,2], label, ...)
}



.arrow <- function(object, xy=click(), length=50000, headlength=0.1, ...) {
	arrows(xy[1], xy[2], xy[1], xy[2]+length, length=headlength, ...)
	lines(rbind(xy, rbind(cbind(xy[1], xy[2]-length))), ...)
	text(xy[1,1], xy[1,2]-(0.25*length), 'N')
}


