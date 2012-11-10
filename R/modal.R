# Author: Robert J. Hijmans 
# Date :  October 2008
# revised: October 2011
# Version 0.9
# Licence GPL v3


setGeneric("modal", function(x, ...)
	standardGeneric("modal"))
	

setMethod('modal', signature(x='ANY'), 
function(x, ..., ties='random', na.rm=FALSE, freq=FALSE) {
#partly based on http://wiki.r-project.org/rwiki/doku.php?id=tips:stats-basic:modalvalue
	x <- c(x, ...)
	z <- x[!is.na(x)]
	
	if (freq) {
	
		if (length(z) == 0) { 
			return(NA) 
		} else if (!na.rm & length(z) < length(x)) { 
			return(NA)	 
		} else if (length(z) == 1) {
			return(1)
		} else {
			return(max( table(z) ))
		}
	}  # else ....
	
	if (!ties %in% c('lowest', 'highest', 'NA', 'random')) {
		stop("the value of 'ties' should be 'lowest', 'highest', 'NA', or 'random'")
	}
	
	if (length(z) == 0) { 
		return(NA) 
	} else if (!na.rm & length(z) < length(x)) { 
		return(NA)	 
	} else if (length(z) == 1) {
		return(z)
	} else {
		freq <- table(z)
		if (is.numeric(z)){
			w <- as.numeric(names(freq[max(freq)==freq]))		
		} else if (is.logical(z)) {
			w <- as.logical(freq[max(freq)==freq])
		} else {
			w <- names(freq[max(freq)==freq])
		}
		if (length(w) > 1) {
			if (ties == 'lowest') {
				w <- min(w)
				if (is.logical(z)) { 
					w <- as.logical(w) 
				}
			} else if (ties == 'highest') {
				w <- max(w)
				if (is.logical(z)) {
					w <- as.logical(w) 
				}
			} else if (ties == 'NA') {
				w <- NA
			} else { # random
				r <- runif(length(w))
				w <- w[which.max(r)]
			}	
		} 
		return(w)
	}	
}
)

