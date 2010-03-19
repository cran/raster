\name{coordinates}

\alias{xmin}
\alias{xmax}
\alias{ymin}
\alias{ymax}
\alias{xmin<-}
\alias{xmax<-}
\alias{ymin<-}
\alias{ymax<-}

  
\title{Coordinates}

\description{
These functions return or set the extreme coordinates of a Raster* object. 
}

\usage{
xmin(object)
xmax(object)
ymin(object)
ymax(object)

xmin(x) <- value
xmax(x) <- value
ymin(x) <- value
ymax(x) <- value

}

\arguments{
  \item{object}{A Raster* object}
  \item{x}{A Raster* object}
  \item{value}{A new x or y coordinate}  
}


\value{
a single number
}

\seealso{ \code{\link[raster]{extent}},  \code{\link[raster]{dimensions}} }


\author{Robert J. Hijmans }

\examples{

r <- raster(xmn=-0.5, xmx = 9.5, ncols=10)
xmin(r)
xmax(r)
ymin(r)
ymax(r)
xmin(r) <- -180
xmax(r) <- 180
}

\keyword{spatial}
