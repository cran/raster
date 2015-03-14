# Author: Robert J. Hijmans
# Date : November 2011
# Version 1.0
# Licence GPL v3


setMethod('aggregate', signature(x='SpatialPolygons'), 
function(x, vars=NULL, sums=NULL, dissolve=TRUE, ...) {

	if (dissolve) {
		stopifnot(requireNamespace("rgeos"))
	}
	
	if (! .hasSlot(x, 'data') ) {
		hd <- FALSE
		if (!is.null(vars)) {
			if (length(vars) == length(x@polygons)) {
				x <- SpatialPolygonsDataFrame(x, data=data.frame(ID=vars))
				vars <- 1
			}
		}
	} else {
		hd <- TRUE
	}
	
	if (isTRUE(is.null(vars))) {
		if (dissolve) {
			if (rgeos::version_GEOS() < "3.3.0") {
				x <- rgeos::gUnionCascaded(x)
			} else {
				x <- rgeos::gUnaryUnion(x)
			}
		} else {
			p <- list()
			for (i in 1:length(x)) {
				nsubobs <- length(x@polygons[[i]]@Polygons)
				p <- c(p, lapply(1:nsubobs, function(j) x@polygons[[i]]@Polygons[[j]]))
			}
			x <- SpatialPolygons(list(Polygons(p, '1')), proj4string=x@proj4string)
		}
		#if (hd) {
		#	x <- SpatialPolygonsDataFrame(x, data=data.frame(ID=1))
		#}
		return(x)
		
	} else {
		getVars <- function(v, cn) {
			vl <- length(v)
			v <- unique(v)
			if (is.numeric(v)) {
				v <- round(v)
				v <- v[v>0 & v <= ncol(x@data)]
				if (length(v) < 1) {
					stop('invalid column numbers')
				}
			} else if (is.character(v)) {
				v <- v[v %in% cn]
				if (length(v) < 1) {
					stop('invalid column names')
				}
			}
			v
		}
		
		dat <- x@data
		cn <- colnames(dat)
		v <- getVars(vars, cn)
		
		dat <- dat[,v, drop=FALSE]
		crs <- x@proj4string
		dc <- apply(dat, 1, function(y) paste(as.character(y), collapse='_'))
		dc <- data.frame(oid=1:length(dc), v=as.integer(as.factor(dc)))
		id <- dc[!duplicated(dc$v), , drop=FALSE]

		if (nrow(id) == nrow(dat)) {
			# nothing to aggregate
			if (hd) {
				x@data <- dat
			} else {
				x <- as(x, 'SpatialPolygons')
			}
			return(x)
		}

		id <- id[order(id$v), ]
		dat <- dat[id[,1], ,drop=FALSE]
		
		if (!is.null(sums)) {
			out <- list()
			for (i in 1:length(sums)) {
				if (length(sums[[i]]) != 2) {
					stop('argument "s" most of be list in which each element is a list of two (fun + varnames)')
				}
				fun = sums[[i]][[1]]
				if (!is.function(fun)) {
					if (is.character(fun)) {
						if (tolower(fun[1]) == 'first') {
							fun <- function(x) x[1]
						} else if  (tolower(fun[1]) == 'last') {
							fun <- function(x) x[length(x)]
						} 
					}
				}
				v <- getVars(sums[[i]][[2]], cn)
				ag <- aggregate(x@data[,v,drop=FALSE], by=list(dc$v), FUN=fun) 
				out[[i]] <- ag[,-1,drop=FALSE]
			}
			out <- do.call(cbind, out)
			dat <- cbind(dat, out)
		}
		
		if (hd) {
			x <- as(x, 'SpatialPolygons')
		}
		if (dissolve) {
			if (rgeos::version_GEOS0() < "3.3.0") {
				x <- lapply(1:nrow(id), function(y) spChFIDs(rgeos::gUnionCascaded(x[dc[dc$v==y,1],]), as.character(y)))
			} else {
				x <- lapply(1:nrow(id), 
						function(y) {
							z <- x[dc[dc$v==y, 1], ]
							try( z <- rgeos::gUnaryUnion(z), silent=TRUE )
							spChFIDs(z, as.character(y))
						} 
					)
			}	
		} else {
			x <- lapply(1:nrow(id), function(y) spChFIDs(aggregate(x[dc[dc$v==y,1],], dissolve=FALSE), as.character(y)))
		}
		
		x <- do.call(rbind, x)
		x@proj4string <- crs
		rownames(dat) <- NULL
		SpatialPolygonsDataFrame(x, dat, FALSE)
	}
}
)


