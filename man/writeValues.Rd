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

\title{Write values to a file}

\description{
Functions for writing blocks (>= 1 row(s)) of values to files. If you want to write all values of a Raster* object at once, you can also use \link[raster]{writeRaster} which is easier to use but more limited. The functions described here allow writing values to file using chunks of different sizes (e.g. 1 or 10 rows). Function \code{\link[raster]{blockSize}} can be used to suggest chunk size.  
}

\usage{
writeStart(x, filename, ...)
writeValues(x, v, ...)
writeStop(x)
}

\arguments{
  \item{x}{Raster* object}
  \item{filename}{Output filename}
  \item{v}{vector (RasterLayer) or matrix (RasterBrick) of values}
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
First open a file with \code{writeStart}, then write values to it in chunks. When writing is done close the file with \code{writeStop}. \code{blockSize} can help set the number of rows of each chunk and it returns the row-numbers from where to start reading.

When writing a file to disk, the file format is determined by the 'format=' argument if supplied, or else by the file extension (if the extension is known). If other cases the default format is used. The 'factory-fresh' default format is 'raster', but this can be changed using \code{\link[raster]{setOptions}}.

}

\value{
RasterLayer or RasterBrick object. 
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{writeRaster}}, \code{\link[raster]{blockSize}} }  

\examples{ 

r <- raster(system.file("external/test.grd", package="raster"))
# write to a new binary file in chunks
s <- raster(r)
# 
tr <- blockSize(r)
tr
s <- writeStart(s, filename='test.grd',  overwrite=TRUE)
for (i in 1:tr$n) {
	v <- getValuesBlock(r, row=tr$row[i], nrows=tr$size)
	s <- writeValues(s, v, tr$row[i])
}
s <- writeStop(s)

if(require(rgdal)){
s2 <- writeStart(s, filename='test2.tif', format='GTiff', overwrite=TRUE)
# writing last row first
for (i in tr$n:1) {
	v <- getValuesBlock(r, row=tr$row[i], nrows=tr$size)
	s2 <- writeValues(s2, v, tr$row[i])
}
# row number 5 once more
v <- getValuesBlock(r, row=5, nrows=1)
writeValues(s2, v, 5)
s2 <- writeStop(s2)
}

## write values of a RasterStack to a RasterBrick
s <- stack(system.file("external/rlogo.grd", package="raster"))
# create empty brick
b <- brick(s, values=FALSE)  
b <- writeStart(b, filename="test.grd", format="raster",overwrite=TRUE)
tr <- blockSize(b)
for (i in 1:tr$n) {
	v <- getValuesBlock(s, row=tr$row[i], nrows=tr$size)
	b <- writeValues(b, v, tr$row[i])
}
b <- writeStop(b)
# note that the above is equivalent to 
# b <- writeRaster(s, filename="test.grd", format="raster",overwrite=TRUE)

}

\keyword{ spatial }
\keyword{ methods }
