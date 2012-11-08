\name{approxNA}

\docType{methods}

\alias{approxNA}
\alias{approxNA,RasterStackBrick-method}

\title{Estimate values for cells that are NA}

\description{
approxNA uses the \code{stats} function \code{\link{approx}} to estimate values for cells that are \code{NA} by interpolation across layers. Layers are considered equidistant, unless an argument 'z' is used, or \code{\link{getZ}} returns values, in which case these values are used to determine distance between layers.

For estimation based on neighboring cells see \code{\link{focal}}
}

\usage{
approxNA(x, ...) 
}

\arguments{
  \item{x}{RasterStack or RasterBrick object}
  \item{...}{additional arguments as in \link[stats]{approxfun} (except for \code{x}, \code{y}, which cannot be used)
and an additional argument 'z' to indicate the distance between layers (e.g., time, depth)  }  
}

\value{
RasterBrick
}

\seealso{ \code{ \link{focal}} } 



\examples{
r <- raster(ncols=5, nrows=5)
r1 <- setValues(r, runif(ncell(r)))
r2 <- setValues(r, runif(ncell(r)))
r3 <- setValues(r, runif(ncell(r)))
r4 <- setValues(r, runif(ncell(r)))
r5 <- setValues(r, NA)
r6 <- setValues(r, runif(ncell(r)))
r1[1:10] <- NA
r2[5:15] <- NA
r3[8:25] <- NA
s <- stack(r1,r2,r3,r4,r5,r6)
x1 <- approxNA(s)
x2 <- approxNA(s, rule=2)

x3 <- approxNA(s, rule=2, z=c(1,2,3,5,14,15))

}

\keyword{spatial}

