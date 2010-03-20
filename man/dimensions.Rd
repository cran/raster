\name{dimensions}

\alias{ncol}
\alias{nrow}
\alias{nrow,BasicRaster-method}
\alias{ncol,BasicRaster-method}
\alias{dim,BasicRaster-method}
\alias{ncell}
\alias{ncell,ANY-method}
\alias{ncell,Raster-method}
\alias{nrow<-}
\alias{ncol<-}
\alias{rowcol<-}


\title{Dimensions of a Raster* object}

\description{
 Get the number of rows, columns, or cells of a Raster* object; or set the number of rows and/columns
}

\usage{
ncol(x)
nrow(x)
ncell(x)
ncol(x) <- value
nrow(x) <- value
rowcol(x) <- value
}


\arguments{
 \item{x}{a Raster* object}
 \item{value}{row or column number or both }  
}

\value{
a number
}

\seealso{ \code{\link[raster]{extent}}, \code{\link[raster]{resolution}} }


\author{ Robert J. Hijmans }


\examples{
r <- raster()
ncell(r)
ncol(r)
nrow(r)
dim(r)

nrow(r) <- 18
ncol(r) <- 36
# equivalent to
rowcol(r) <- c(18, 36) 
}

\keyword{spatial}

