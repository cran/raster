\name{data}

\alias{fromDisk}
\alias{inMemory}
\alias{dataContent}

\title{Are values in memory and/or on disk?}

\description{ 
These are helper functons for programmers and for debugging that provide information about the data values that are associated with the object. 

\code{fromDisk} is \code{TRUE} if the data source is a file on disk; and \code{FALSE} if it only exists in memory. 
\code{inMemory} \code{TRUE} if all values are currently in memory (RAM); and \code{FALSE} if not (in which case they either are on disk, or there are no values).
}

\usage{
fromDisk(object)
inMemory(object)
dataContent(object)
}

\arguments{
  \item{object}{ Raster* object }
}

\value{
Logical value
}

\author{Robert J. Hijmans}

\examples{
rs <- raster(system.file("external/test.grd", package="raster"))
inMemory(rs)
fromDisk(rs)
rs <- readAll(rs)
inMemory(rs)
fromDisk(rs)
rs <- rs + 1
inMemory(rs)
fromDisk(rs)
rs <- raster(rs)
inMemory(rs)
fromDisk(rs)
rs <- setValues(rs, 1:ncell(rs))
inMemory(rs)
fromDisk(rs)
rs <- writeRaster(rs, filename='test', overwrite=TRUE)
inMemory(rs)
fromDisk(rs)
}

\keyword{ spatial }
