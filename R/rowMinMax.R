# minor changes from function by Sebastian Kranz <skr...@uni-bonn.de>
#http://www.mail-archive.com/r-devel@r-project.org/msg20044.html

.rowMin <- function(x, na.rm=TRUE) {
  .Call("minmax", as.double(t(x)), as.integer(c(NROW(x), NCOL(x), 0, isTRUE(na.rm))), PACKAGE='raster')
}

.rowMax <- function(x, na.rm=TRUE) {
  .Call("minmax", as.double(t(x)), as.integer(c(NROW(x), NCOL(x), 1, isTRUE(na.rm))), PACKAGE='raster')
}

.colMin <- function(x, na.rm=TRUE) {
  .Call("minmax", as.double(x), as.integer(c(NCOL(x), NROW(x), 0, isTRUE(na.rm))), PACKAGE='raster')
}

.colMax <- function(x, na.rm=TRUE) {
   .Call("minmax", as.double(x), as.integer(c(NCOL(x), NROW(x), 1, isTRUE(na.rm))), PACKAGE='raster')
}


