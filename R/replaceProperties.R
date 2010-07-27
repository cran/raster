# Author: Robert J. Hijmans, r.hijmans@gmail.com
# International Rice Research Institute
# Date :  January 2009
# Version 0.9
# Licence GPL v3



'ncol<-' <- function(x, value) {
	rowcol(x) <- c(nrow(x), value)
	return(x)
}	

'nrow<-' <- function(x, value) {
	rowcol(x) <- c(value, ncol(x))
	return(x)
}	


'xmin<-' <- function(x, value) {
	x@extent@xmin <- value
	return(x)
}

'xmax<-' <- function(x, value) {
	x@extent@xmax <- value
	return(x)
}

'ymin<-' <- function(x, value) {
	x@extent@ymin <- value
	return(x)
}

'ymax<-' <- function(x, value) {
	x@extent@ymax <- value
	return(x)
}

