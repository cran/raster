
.rowMin <- function(x, na.rm=TRUE) {
  #.Call("minmax", as.double(t(x)), as.integer(c(NROW(x), NCOL(x), 0, isTRUE(na.rm))), PACKAGE='raster')
  apply(x, 1, min, na.rm=na.rm)
}

.rowMax <- function(x, na.rm=TRUE) {
  #.Call("minmax", as.double(t(x)), as.integer(c(NROW(x), NCOL(x), 1, isTRUE(na.rm))), PACKAGE='raster')
   apply(x, 1, max, na.rm=na.rm)
}

.colMin <- function(x, na.rm=TRUE) {
  #.Call("minmax", as.double(x), as.integer(c(NCOL(x), NROW(x), 0, isTRUE(na.rm))), PACKAGE='raster')
   apply(x, 2, min, na.rm=na.rm)
}

.colMax <- function(x, na.rm=TRUE) {
   #.Call("minmax", as.double(x), as.integer(c(NCOL(x), NROW(x), 1, isTRUE(na.rm))), PACKAGE='raster')
   apply(x, 2, max, na.rm=na.rm)   
}


