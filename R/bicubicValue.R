# Interpolates the value of a point in a two dimensional surface using bicubic interpolation.
# The value is calculated using the position of the point and the values of the 16 surrounding points.
#  Note that the returned value can be more or less than any of the values of the surrounding points. 
 
# @param p A 4x4 array containing the values of the 16 surrounding points
#  @param x The horizontal distance between the point and the four points left of it, between 0 and 1
# @param y The vertical distance between the point and the four points below it, between 0 and 1
# @return the interpolated value


.bicinter <- function(p, x, y) {

	a00 <- p[2, 2]
	a01 <- -.5*p[2, 1] + .5*p[2, 3]
	a02 <- p[2, 1] - 2.5*p[2, 2] + 2*p[2, 3] - .5*p[2, 4]
	a03 <- -.5*p[2, 1] + 1.5*p[2, 2] - 1.5*p[2, 3] + .5*p[2, 4]
	a10 <- -.5*p[1, 2] + .5*p[3, 2]
	a11 <- .25*p[1, 1] - .25*p[1, 3] - .25*p[3, 1] + .25*p[3, 3]
	a12 <- -.5*p[1, 1] + 1.25*p[1, 2] - p[1, 3] + .25*p[1, 4] + .5*p[3, 1] - 1.25*p[3, 2] + p[3, 3] - .25*p[3, 4]
	a13 <- .25*p[1, 1] - .75*p[1, 2] + .75*p[1, 3] - .25*p[1, 4] - .25*p[3, 1] + .75*p[3, 2] - .75*p[3, 3] + .25*p[3, 4]
	a20 <- p[1, 2] - 2.5*p[2, 2] + 2*p[3, 2] - .5*p[4, 2]
	a21 <- -.5*p[1, 1] + .5*p[1, 3] + 1.25*p[2, 1] - 1.25*p[2, 3] - p[3, 1] + p[3, 3] + .25*p[4, 1] - .25*p[4, 3]
	a22 <- p[1, 1] - 2.5*p[1, 2] + 2*p[1, 3] - .5*p[1, 4] - 2.5*p[2, 1] + 6.25*p[2, 2] - 5*p[2, 3] + 1.25*p[2, 4] + 2*p[3, 1] - 5*p[3, 2] + 4*p[3, 3] - p[3, 4] - .5*p[4, 1] + 1.25*p[4, 2] - p[4, 3] + .25*p[4, 4]
	a23 <- -.5*p[1, 1] + 1.5*p[1, 2] - 1.5*p[1, 3] + .5*p[1, 4] + 1.25*p[2, 1] - 3.75*p[2, 2] + 3.75*p[2, 3] - 1.25*p[2, 4] - p[3, 1] + 3*p[3, 2] - 3*p[3, 3] + p[3, 4] + .25*p[4, 1] - .75*p[4, 2] + .75*p[4, 3] - .25*p[4, 4]
	a30 <- -.5*p[1, 2] + 1.5*p[2, 2] - 1.5*p[3, 2] + .5*p[4, 2]
	a31 <- .25*p[1, 1] - .25*p[1, 3] - .75*p[2, 1] + .75*p[2, 3] + .75*p[3, 1] - .75*p[3, 3] - .25*p[4, 1] + .25*p[4, 3]
	a32 <- -.5*p[1, 1] + 1.25*p[1, 2] - p[1, 3] + .25*p[1, 4] + 1.5*p[2, 1] - 3.75*p[2, 2] + 3*p[2, 3] - .75*p[2, 4] - 1.5*p[3, 1] + 3.75*p[3, 2] - 3*p[3, 3] + .75*p[3, 4] + .5*p[4, 1] - 1.25*p[4, 2] + p[4, 3] - .25*p[4, 4]
	a33 <- .25*p[1, 1] - .75*p[1, 2] + .75*p[1, 3] - .25*p[1, 4] - .75*p[2, 1] + 2.25*p[2, 2] - 2.25*p[2, 3] + .75*p[2, 4] + .75*p[3, 1] - 2.25*p[3, 2] + 2.25*p[3, 3] - .75*p[3, 4] - .25*p[4, 1] + .75*p[4, 2] - .75*p[4, 3] + .25*p[4, 4]

	x2 <- x * x
	x3 <- x2 * x
	y2 <- y * y
	y3 <- y2 * y

	return (a00 + a01 * y + a02 * y2 + a03 * y3 +
	       a10 * x + a11 * x * y + a12 * x * y2 + a13 * x * y3 +
	       a20 * x2 + a21 * x2 * y + a22 * x2 * y2 + a23 * x2 * y3 +
	       a30 * x3 + a31 * x3 * y + a32 * x3 * y2 + a33 * x3 * y3)
}



.bicubicInterpolate <- function(r, xy) {

	warning("not tested")
	row <- rowFromY(r, xy[,2])
	col <- colFromX(r, xy[,1])
	cell <- cellFromRowCol(r, row, col)
	cxy <- xyFromCell(r, cell)
	dis <- cxy - xy
	x <- (abs(dis[,1]) / xres(r))
	y <- (abs(dis[,2]) / yres(r))

	dCtr <- dis < 0
	
	value <- vector(length=nrow(xy))
	for (i in 1:nrow(xy)) {
	# 5x5 window
		m <- getValuesBlock(r, row=row[i]+2-dCtr[i,1], nrows=4, col=col[i]+2-dCtr[i,2], ncols=4)
		m <- matrix(m, ncol=4, byrow=T)
		value[i] <- .bicinter(m, x[i], y[i])
	}
	return(value)
}
