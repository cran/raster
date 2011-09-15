# Author: Robert J. Hijmans
# Date : April 2011
# Version 1.0
# Licence GPL v3


..moran <- function(x, directions=8) {
	stopifnot(directions %in% c(4,8))
	# not memory safe	
	adj <- adjacency(x, fromCells=1:ncell(x), toCells=1:ncell(x), directions=8)
	z <- x - cellStats(x, mean)
	wZiZj <- na.omit(z[adj[,1]] * z[adj[,2]])
	z2 <- cellStats(z*z, sum)
	NS0 <- (ncell(z)-cellStats(z, 'countNA')) / length(wZiZj)
	mI <- NS0 * sum(wZiZj) / z2
	return(mI)
}



Moran <- function(x, w=3) {

	z <- x - cellStats(x, mean)
	w <- .getFilter(w)
	wZiZj <- focalFilter(z, filter=w, fun=sum, na.rm=TRUE, pad=TRUE)
	wZiZj <- overlay(wZiZj, z, fun=function(x,y){ x * y })
	wZiZj <- cellStats(wZiZj, sum)
	z2 <- cellStats(z*z, sum)
	n <- ncell(z) - cellStats(z, 'countNA')
	# weights
	if (sum(! unique(w) %in% 0:1) > 0) {
		zz <- calc(z, fun=function(x) ifelse(is.na(x), NA ,1))
		W <- focalFilter( zz, filter=w, fun=sum, na.rm=TRUE, pad=TRUE) 
	} else {
		w2 <- w
		w2[w2==0] <- NA
		W <- focalFilter( z, filter=w2, fun=function(x, ...){  sum(!is.na(x)) }, pad=TRUE)
	}
	NS0 <- n / cellStats(W, sum)
	mI <- NS0 * wZiZj / z2
	return(mI)
}


MoranLocal <- function(x, w=3) { 
	
	z  <- x - cellStats(x, mean) 
	#weights
	w <- .getFilter(w)
	if (sum(! unique(w) %in% 0:1) > 0) {
		zz <- calc(z, fun=function(x) ifelse(is.na(x), NA ,1))
		W  <- focalFilter( zz, filter=w, fun=sum, na.rm=TRUE, pad=TRUE)
	} else {
		w2 <- w
		w2[w2==0] <- NA
		W  <- focalFilter( z, filter=w2, fun=function(x, ...){ sum(!is.na(x)) }, na.rm=TRUE, pad=TRUE)
	}
	lz <- focalFilter(z, filter=w, fun=sum, na.rm=TRUE, pad=TRUE) / W
		
	n <- ncell(x) - cellStats(x, 'countNA')
	s2 <-  cellStats(x, sd)^2 
	# adjust variance denominator from n-1 to n 
	s2 <- (s2 * (n-1)) / n 

	(z / s2) * lz
} 


