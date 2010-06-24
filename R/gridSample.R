# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date : June 2009
# Version 1
# Licence GPL v3


gridSample <- function(xy, r, n=1, chess='') {

	if (inherits(xy, 'Spatial')) {
		xy <- coordinates(xy)
	}
    cell = cellFromXY(r, xy)
	cell = na.omit(cell)
    uc = unique(cell)
	
	if (chess != '') {
		chess = tolower(chess)
		if (chess == 'white') {
			tf <- 1:ceiling(ncell(r)/2) * 2 - 1
		} else if (chess == 'black') {
			tf <- 1:ceiling(ncell(r)/2) * 2
		} else {
			stop("The value of chess should be '', 'black' or 'white'")
		}	
		uc <- uc[uc %in% tf]
	}
	
    xy = cbind(xy, cell, runif( nrow(xy)))
    xy =  xy[order(xy[,4]), ]
    pts = matrix(nrow=0, ncol=2)
    for (u in uc) {
        ss = subset(xy, xy[,3] == u)
        pts = rbind(pts, ss[1:min(n, nrow(ss)), 1:2])
    }
    return(pts)
	
}

