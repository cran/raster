\name{sampleRandom}

\alias{sampleRandom}
\alias{sampleRandom,Raster-method}


\title{Random sample}

\description{
Take a random sample from a Raster* object. 
}

\usage{
sampleRandom(x, size, ...)
}

\arguments{
  \item{x}{a Raster object}
  \item{size}{positive integer giving the number of items to choose.}  
  \item{...}{Additional arguments: 
  
	\code{na.rm}  Logical. If \code{TRUE} (the default), \code{NA} values are removed from random sample
	
	\code{ext}  Extent. To limit regular sampling to the area within the extent  
	
	\code{cells}  Logical. If \code{TRUE}, sampled cell numbers are also returned
	
	\code{rowcol}  Logical. If \code{TRUE}, sampled row and column numbers are also returned

	\code{sp}  Logical. If \code{TRUE}, a SpatialPointsDataFrame is returned
	
	\code{asRaster}  Logical. If \code{TRUE}, a Raster* object is returned with random cells with values, all other cells with \code{NA}
	
	}
}

\details{
With \code{na.rm=TRUE}, the returned sample may be smaller than requested
}

\value{
A vector, matrix (if \code{cells=TRUE} or \code{x} is a multi-layered object), or a SpatialPointsDataFrame (if \code{sp=TRUE} )
}

\seealso{\code{\link{sampleRegular}}}

\author{Robert J. Hijmans}

\examples{
 r <- raster(system.file("external/test.grd", package="raster"))
 sampleRandom(r, size=10)
 s <- stack(r, r)
 sampleRandom(s, size=10, cells=TRUE, sp=TRUE)
}

\keyword{spatial}
