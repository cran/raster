\name{rowSums}

\docType{methods}

\alias{rowSums}
\alias{rowSums,Raster-method}
\alias{colSums}
\alias{colSums,Raster-method}

\title{rowSum and colSum for Raster objects}

\description{
Sum row or clolumn values of Raster objects
}

\usage{
\S4method{rowSums}{Raster}(x, na.rm=FALSE, dims=1L,...) 
\S4method{colSums}{Raster}(x, na.rm=FALSE, dims=1L,...) 
}

\arguments{
  \item{x}{Raster* object}
  \item{na.rm}{logical. If \code{TRUE}, \code{NA} values are ignored}
  \item{dims}{this argument is ignored}
  \item{...}{additional arguments (none implemented)}
}

\value{
vector (if \code{x} is a RasterLayer) or matrix
}


\examples{
r <- raster(ncols=2, nrows=5)
values(r) <- 1:10
as.matrix(r)
rowSums(r)
colSums(r)
}

\keyword{spatial}

