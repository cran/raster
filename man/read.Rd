\name{read}

\alias{readAll,RasterLayer-method}
\alias{readAll,RasterStack-method}
\alias{readAll,RasterBrick-method}
\alias{readRow,RasterLayer-method}
\alias{readRow,RasterStack-method}
\alias{readRow,RasterBrick-method}
\alias{readRows,RasterLayer-method}
\alias{readRows,RasterStack-method}
\alias{readRows,RasterBrick-method}
\alias{readBlock,RasterLayer-method}
\alias{readBlock,RasterStack-method}
\alias{readBlock,RasterBrick-method}

\alias{readAll}
\alias{readRow}
\alias{readRows}
\alias{readBlock}

\title{Read values from disk}

\description{
Read values from a raster file associated with a Raster* object into memory.
You can either read all the data (memory permitting), or read data for a single or multiple rows, or a block (a rectangular area). 
read* functions will fail when there is no file associated with the RasterLayer (values may only exist on memory). It is therefore safer to use
\code{\link[raster]{getValues}}.
}


\usage{
readAll(object)
readRow(object, rownr)
readRows(object, startrow, nrows)
readBlock(object, startrow, nrows=3, startcol=1, ncolumns=(ncol(object)-startcol+1)) 
}


\arguments{
  \item{object}{a Raster* object}
  \item{rownr}{the row number of the row to read  (between 1 and nrows(raster))}
  \item{startrow}{the frist row to read  (>= 1 & <= nrows(raster))}
  \item{nrows}{the number of the rows to read (>= 1 & <= nrows(raster))}
  \item{startcol}{first column of the block ( (>= 1 & <= nrows(raster))}
  \item{ncolumns}{number of columns to read on this each row ( (>= 1 & <= ncols(raster) & > firstcol)}
}


\note{After read* the values are accessible with the "values(object)" function}

\seealso{ \code{\link[raster]{getValues}}, \code{\link[raster]{xyValues}}, \code{\link[raster]{cellValues}}, \code{\link[raster]{writeRaster}}}

\author{Robert J. Hijmans}

\examples{

 rs <- raster(system.file("external/test.grd", package="raster"))

# read a row
  firstrow <- readRow(rs, 1)
  lastrow <- readRow(rs, rs@nrows)

# read a number of rows
  fiverows <- readRows(rs, startrow=50, nrows=5)

# read a block
  block <- readBlock(rs, startrow=50, nrows=5, startcol=30, ncolumns=10)
  
# read a row from a stack
  st <- stack(rs, rs, rs)  
  st <- readRow(st, 50)
}

\keyword{classes}
\keyword{spatial}
