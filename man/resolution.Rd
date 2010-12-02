\name{resolution}

\alias{xres}
\alias{yres}
\alias{res}
\alias{res<-}


\title{Resolution}

\description{
  Get resolution properties of a Raster* object
}

\usage{
xres(object)
yres(object)
res(object)
res(object) <- value
}

\arguments{
  \item{object}{Raster* object}
  \item{value}{depends on context. Can be row or column number or resolution }  
}


\value{
A single numeric value or two numeric values.
}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{ncell}}  }


\author{Robert J. Hijmans }
\examples{
r <- raster(ncol=18, nrow=18)
xres(r)
yres(r)
res(r)

res(r) <- 1/120
# set yres diffrently
res(r) <- c(1/120, 1/60)


}

\keyword{spatial}
