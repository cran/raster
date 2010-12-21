# Author: Robert J. Hijmans, r.hijmans@gmail.com
# Date :  June 2008
# Version 0.9
# Licence GPL v3

.addHistory <- function(x, txt) {
	txt <- trim(as.character(txt))
	if (txt != "") {
		x@history <- c(txt, x@history)
	}	
}

