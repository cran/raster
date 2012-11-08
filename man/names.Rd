\name{names}

\alias{layerNames}
\alias{layerNames<-}
\alias{names}
\alias{names<-}
\alias{names,Raster-method}
\alias{names,RasterStack-method}
\alias{names<-,Raster-method}

  
\title{Names of raster layers}

\description{
Get or set the names of the layers of a Raster* object

Function \code{layerNames} is deprecated. Use \code{names} instead. 
}

\usage{
\S4method{names}{Raster}(x)

\S4method{names}{Raster}(x)<-value

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





\examples{
r <- raster(ncols=5, nrows=5)
r[] <- 1:ncell(r)
s <- stack(r, r, r)
nlayers(s)
names(s)
names(s) <- c('a', 'b', 'c')
names(s)[2] <- 'hello world'
names(s)
s
}

\keyword{spatial}
