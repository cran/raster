# Author: Robert J. Hijmans 
# r.hijmans@gmail.com
# Date : October 2008
# Licence GPL v3


setMethod('quantile', signature(x='Raster'), 
	function(x, ..., na.rm=TRUE, ncells=NULL) {
		if (is.null(ncells)) {
			v <- try ( getValues(x) )
			if (class(v) == 'try-error') {
				stop('raster too large. You can use an argument "ncells" to use a sample of the cells')
			}
		} else {
			if (ncells >= ncell(x)) {
				v <- try ( getValues(x) )
			} else {
				v <- try ( sampleRandom(x, ncells) ) 
			}
			if (class(v) == 'try-error') {
				stop('ncells too large')
			}
		}
		if (na.rm) {
			v <- na.omit(v)
		}
		return(quantile(v, ...))
	}
)

