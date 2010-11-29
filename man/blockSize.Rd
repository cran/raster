\name{blockSize}


\alias{blockSize}

\title{Block size for writing files}

\description{
This function can be used to suggest a chunk size, and corresponding row numbers, to be used when processing Raster objects chunk by chunk, normally together with \code{\link{writeValues}}.
}

\usage{
blockSize(x, chunksize, n=nlayers(x), minblocks=2)
}

\arguments{
  \item{x}{Raster* object}
  \item{chunksize}{Integer, normally missing. Can be used to set the block size; unit is number of cells. Block size is then computed in units of number of rows (always >= 1)  }
  \item{n}{Integer. number of layers to consider. The function divides chunksize by n to determine blocksize }
  \item{minblocks}{Integer. Minimum number of blocks }
}


\value{
A list with \code{rows}, the suggested row numbers at which to start the blocks for reading and writing, \code{size}, the block size (number of rows) and \code{n}, the total number of blocks 
}

\author{Robert J. Hijmans}

\seealso{ \code{\link[raster]{writeValues}} }  

\examples{ 

r <- raster(system.file("external/test.grd", package="raster"))
blockSize(r)
}

\keyword{ spatial }
\keyword{ methods }
