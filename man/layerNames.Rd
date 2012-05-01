\name{layerNames}

\alias{layerNames}
\alias{layerNames<-}
  
\title{layerNames}

\description{
Get or set layer names of a Raster* object
}

\usage{
layerNames(x)
layerNames(x) <- value
}

\arguments{
  \item{x}{Raster* object}
  \item{value}{Character (vector)}
}

\value{
Character 
}

\seealso{ \code{\link{nlayers}, \link[raster]{bands}} }


\author{Robert J. Hijmans }
\examples{
r <- raster(ncols=10, nrows=10)
r[] <- 1:ncell(r)
s <- stack(r, r, r)
nlayers(s)
layerNames(s)
layerNames(s) <- c('a', 'b', 'c')
layerNames(s)[2] <- 'hello world'
layerNames(s)
s
}

\keyword{spatial}
