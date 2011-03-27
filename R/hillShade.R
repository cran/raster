# Author: Andrew Bevan and Robert J. Hijmans
# Date : March 2010
# Version 1.0
# Licence GPL v3


hillShade <- function(slope, aspect, declination, direction, filename='', ...) {
	compare(slope, aspect)
	declination <- declination * pi/180
	direction <- direction * pi/180
	x <- cos(slope) * cos(declination) + sin(slope) * sin(declination) * cos(direction-aspect)
	if (filename != "") {
		x <- writeRaster(x, filename, ...)
	}
	return(x)
}


.hillShade2 <- function(slope, aspect, declination, direction, filename='', ...) {
	declination <- (90-declination) * (pi / 180) #in radians
    direction <- 360 - direction + 90 #anti-clockwise from east
	if (direction>=360) { direction <- direction - 360 }
	direction <- direction * (pi / 180) #in radians
	x <-((cos(declination)*cos(slope)) + (sin(declination)*sin(slope) * cos(direction-aspect)))
	if (filename != "") {
		x <- writeRaster(x, filename, ...)
	}
	return(x)
}

