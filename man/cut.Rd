\name{cut}

\docType{methods}

\alias{cut}
\alias{cut,Raster-method}

\title{Convert values to classes}

\description{
Cut uses the base function \code{\link[base]{cut}} to divides the range of the values of a Raster* object into intervals and codes the values in x according to which interval they fall. The leftmost interval corresponds to level one, the next leftmost to level two and so on.
}

\usage{
cut(x, ...) 
}

\arguments{
  \item{x}{A Raster* object}
  \item{...}{additional arguments. See \link[base]{cut}}  
}

\value{
Raster* object
}

\seealso{ \code{ \link{subs}, \link{reclass}, \link{calc}} } 


\author{Robert J. Hijmans}

\examples{

r <- raster(ncols=36, nrows=18)
r[] <- rnorm(ncell(r)) 
breaks=-2:2 * 3
rc <- cut(r, breaks=breaks)
}

\keyword{spatial}
