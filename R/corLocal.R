# Author: Robert J. Hijmans
# Date : February 2014
# Version 1.0
# Licence GPL v3


if ( !isGeneric("corLocal") ) {
	setGeneric("corLocal", function(x, y, ...)
		standardGeneric("corLocal"))
}


setMethod('corLocal', signature(x='RasterLayer', y='RasterLayer'), 
	function(x, y, ngb=5, method = c("pearson", "kendall", "spearman"), test=FALSE, filename='', ...) {
		
		compareRaster(x,y)
		if (test) {
			out <- brick(x, values=FALSE, nl=2)
			names(out) <- c(method[1], 'p-value')
		} else {
			out <- raster(x)
			names(out) <- c(method[1])		
		}
		
		if (canProcessInMemory(x)) {
			vx <- getValuesFocal(x, 1, nrow(x), ngb=ngb)
			vy <- getValuesFocal(y, 1, nrow(y), ngb=ngb)
			if (test)  {
				v <- matrix(NA, ncol=2, nrow=ncell(x))
				for (i in 1:ncell(x)) {
					z <- na.omit(cbind(vx[i,], vy[i,]))	
					if (nrow(z) > 3) {
						a <- cor.test(z[,1], z[,2], method=method)
						v[i, ] <- c(a$estimate, a$p.value)
					}
				}
			} else {
				v <- rep(NA, nrow=ncell(x))
				for (i in 1:ncell(x)) {
					z <- na.omit(cbind(vx[i,], vy[i,]))	
					if (nrow(z) > 3) {
						v[i] <- cor(z[,1], z[,2], method=method)
					}
				}
			}
			out <- setValues(out, v)
			if (filename != '') {
				out <- writeRaster(out, filename, ...)
			}
			return(out)
			
		} else {
			tr <- blockSize(out)
			pb <- pbCreate(tr$n, label='corLocal', ...)
			out <- writeStart(out, filename=filename, ...)
			if (test) {
				for (i in 1:tr$n) {
					vx <- getValuesFocal(x, tr$row[i], tr$nrows[i], ngb=ngb)
					vy <- getValuesFocal(y, tr$row[i], tr$nrows[i], ngb=ngb)
					v <- matrix(NA, ncol=2, nrow=nrow(vx))
					for (j in 1:nrow(vx)) {
						z <- na.omit(cbind(vx[j,], vy[j,]))	
						if (nrow(z) > 3) {
							a <- cor.test(z[,1], z[,2], method=method)
							v[j, ] <- c(a$estimate, a$p.value)
						}
					}
					out <- writeValues(out, v, tr$row[i])
				}
			} else {
				for (i in 1:tr$n) {
					vx <- getValuesFocal(x, tr$row[i], tr$nrows[i], ngb=ngb)
					vy <- getValuesFocal(y, tr$row[i], tr$nrows[i], ngb=ngb)
					v <- rep(NA, nrow(vx))
					for (j in 1:length(v)) {
						z <- na.omit(cbind(vx[j,], vy[j,]))	
						if (nrow(z) > 3) {
							v[j] <- cor(z[,1], z[,2], method=method)
						}
					}
					out <- writeValues(out, v, tr$row[i])
				}
			}
			return(writeStop(out))
		}
	}
)

