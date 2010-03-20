\name{writeValues}


\alias{writeStart}
\alias{writeStart,RasterLayer,character-method}
\alias{writeStart,RasterBrick,character-method}
\alias{writeStop}
\alias{writeStop,RasterLayer-method}
\alias{writeStop,RasterBrick-method}
\alias{writeValues}
\alias{writeValues,RasterLayer-method}
\alias{writeValues,RasterBrick-method}
\alias{blockSize}

\title{Write values to a file}

\description{
Low level functions for writing blocks (>= 1 row) of values to files. For programmers and advanced users to speed up functions. Others are adviced to use \link[raster]{writeRaster} which is safer but more limited. writeRaster can only take all values (for smaller data sets) or a single row at a time (for larger data sets). The advantage of the functions described here is that they allow writing values to file using chunks of any size (and some additional optimization for speed). Function \code{blockSize} can be used to suggest chunks of a number of rows that can be much larger than 1 (up to all rows). This reduces the number of loops and the number of file i/o requests and hence could speed up writing values to file. These functions do not have many validity tests and will try to do as requested, or perhaps fail, or write invalid files. Hence the adviced to use \link[raster]{writeRaster} instead (or first, and later optimize with the functions described here). 
}

\usage{
blockSize(x, chunksize, n=1, minblocks=2)
writeStart(x, filename, ...)
writeValues(x, v, ...)
writeStop(x)
}

\arguments{
  \item{x}{Raster* object}
  \item{chunksize}{Integer, normally missing. Can be used to set the block size; unit is number of cells. Block size is then computed in units of number of rows (always >= 1)  }
  \item{n}{Integer. number of layers to consider. The function divides chunksize by n to determine blocksize }
  \item{minblocks}{Integer. Minimum number of blocks }
  \item{filename}{Output filename}
  \item{v}{vector}
  \item{...}{Additional arguments. See under Methods}    
}

\section{Methods}{
\describe{
\bold{Additional arguments for writeStart}
\tabular{rll}{
\tab \code{format} \tab Character. Output file type. See \code{\link[raster]{writeFormats}} \cr
\tab \code{datatype} \tab Character. Output data type. See \code{\link[raster]{dataType}} \cr
\tab \code{overwrite} \tab Logical. If \code{TRUE}, "filename" will be overwritten if it exists \cr
}

}}

\details{
First open a file with \code{writeStart}, then write values to it in chunks. When writing is done close the file with \code{writeStop}. \code{chunkSize} can help set the number of rows of each chunk and it returns the row-numbers from where to start reading.

Files with a 'native' format must be written in sequence of cell numbers. In contrast, the values of files written via rgdal can be written in any order (but you must write 1 or more entire rows, you cannot write part of a row). For files written via rgdal, an argument \code{start} must be provided to \code{writeValues} so that it knows where to write the values. This argument is ignored for the native formats (values are assumed to be in sequence). 
}

\value{
the write functions are used for the side-effect of writing values to a file.

tileSize returns a list with \code{rows}, the suggested row numbers at which to start the blocks for reading and writing, \code{size}, the block size (number of rows) and \code{n}, the total number of blocks 
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{writeRaster}} }  

\examples{ 

r <- raster(system.file("external/test.grd", package="raster"))
# write to a new binary file in chunks
s <- raster(r)
# 
tr <- blockSize(r, 25)
tr
s <- writeStart(s, filename='test.grd',  overwrite=TRUE)
for (i in 1:tr$n) {
	v <- getValuesBlock(r, row=tr$row[i], nrows=tr$size)
	writeValues(s, v, tr$row[i])
}
s <- writeStop(s)

s2 <- writeStart(s, filename='test2.tif', format='GTiff', overwrite=TRUE)
# writing last row first
for (i in tr$n:1) {
	v <- getValuesBlock(r, row=tr$row[i], nrows=tr$size)
	writeValues(s2, v, tr$row[i])
}
# row number 5 once more
v <- getValuesBlock(r, row=5, nrows=1)
writeValues(s2, v, 5)
s2 <- writeStop(s2)

}

\keyword{ spatial }
\keyword{ methods }
